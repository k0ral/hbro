{-# LANGUAGE FlexibleInstances, TypeFamilies #-}
module Hbro.Core where

-- {{{ Imports
import qualified Hbro.Clipboard as Clipboard
import Hbro.Config
import Hbro.Error
import Hbro.Gtk.ScrolledWindow (Axis(..), Position(..))
import qualified Hbro.Gtk.ScrolledWindow as SW
import Hbro.Gui as GUI
import qualified Hbro.Keys as Keys
import Hbro.IPC
import Hbro.Network
import Hbro.Notification
import Hbro.Options (CliOptions, OptionsReader)
import qualified Hbro.Options as Options
import Hbro.Prompt (PromptReader)
import qualified Hbro.Prompt as Prompt
import Hbro.Util as H
import qualified Hbro.Webkit.WebView as W

import Control.Applicative
import Control.Conditional hiding(unless)
import Control.Lens hiding((??))
import Control.Monad
import Control.Monad.Base
import Control.Monad.Error  hiding(forM_, mapM_, unless)
import Control.Monad.Reader hiding(forM_, mapM_, unless)
import Control.Monad.Writer hiding(forM_, mapM_, unless)
import Control.Monad.Trans.Control

import Data.Default
-- import Data.Foldable
-- import Data.Functor
import Data.IORef
import Data.Maybe
import qualified Data.Map as M

import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.General.General
import Graphics.UI.Gtk.WebKit.WebDataSource
import Graphics.UI.Gtk.WebKit.WebFrame as W
import Graphics.UI.Gtk.WebKit.WebInspector
import Graphics.UI.Gtk.WebKit.WebView as W
import Graphics.UI.Gtk.Windows.Window

import Network.URI (URI, uriScheme)
import qualified Network.URI as N

import Prelude hiding(concat, mapM_)

import qualified System.Glib.Attributes as G
-- }}}

-- {{{ Types
--newtype (Monad m) => KT m a = KT { unKT :: ErrorT HError (WriterT String (ReaderT CliOptions (ReaderT (IORef (Config (KT m))) (ReaderT (GUI (KT m)) (ReaderT IPC m))))) a}
--    deriving (Applicative, Functor, Monad, MonadWriter String)
--type K = KT IO
newtype K a = K { unKT :: ErrorT HError (WriterT String (ReaderT CliOptions (ReaderT (IORef (Config K)) (ReaderT (GUI K) (ReaderT IPC (ReaderT (IORef Keys.Status) IO)))))) a}
    deriving (Applicative, Functor, Monad, MonadBase IO, MonadError HError, MonadWriter String)

instance MonadBaseControl IO K where
    newtype StM K a  = StK { unStK :: StM (ErrorT HError (WriterT String (ReaderT CliOptions (ReaderT (IORef (Config K)) (ReaderT (GUI K) (ReaderT IPC (ReaderT (IORef Keys.Status) IO))))))) a }
    liftBaseWith f   = K . liftBaseWith $ \runInBase -> f $ liftM StK . runInBase . unKT
    restoreM         = K . restoreM . unStK

instance ConfigReader K K where
    readConfig l = K $ (lift . lift . lift) ask >>= io . readIORef >>= return . view l

instance ConfigWriter K K where
    writeConfig l v = K $ (lift . lift . lift) ask >>= io . (`modifyIORef` set l v)

instance GUIReader K K where
    readGUI l = K $ (lift . lift . lift . lift) ask >>= return . view l

instance IPCReader K where
    readIPC l = K $ (lift . lift . lift . lift . lift) ask >>= return . view l

instance NotificationReader K where
    readNotification l = K $ (lift . lift . lift . lift) ask >>= return . view (notificationBar.l)

instance OptionsReader K where
    readOptions l = K $ (lift . lift) ask >>= return . view l

instance PromptReader K K where
    readPrompt l = K $ (lift . lift . lift . lift) ask >>= return . view (promptBar.l)

instance Keys.StatusReader K where
    readStatus l = K $ (lift . lift . lift . lift . lift . lift) ask >>= io . readIORef >>= return . view l

instance Keys.StatusWriter K where
    writeStatus l v = K $ (lift . lift . lift . lift . lift . lift) ask >>= io . (`modifyIORef` set l v)

runK :: CliOptions -> Config K -> GUI K -> IPC -> K a -> IO ((Either HError a), String)
runK options config gui ipc k = do
    config'    <- newIORef config
    keysStatus <- newIORef def
    (`runReaderT` keysStatus) . (`runReaderT` ipc) . (`runReaderT` gui) . (`runReaderT` config'). (`runReaderT` options) . runWriterT . runErrorT $ unKT k


data CaseSensitivity = CaseSensitive | CaseInsensitive
data Direction       = Forward       | Backward
data Wrap            = Wrap          | NoWrap
data ZoomDirection   = In            | Out
-- }}}


-- {{{ Default configuration
instance Default (Config K) where
    def = Config {
        _homePage         = fromJust . N.parseURI $ "https://duckduckgo.com/",
        _verbosity        = Normal,
        _keyBindings      = defaultKeyBindings,
        _commands         = def,
        _onDownload       = defaultDownload,
        _onKeyStroke      = const $ return (),
        _onLinkClicked    = defaultLinkClicked,
        _onLoadFinished   = return (),
        _onLoadRequested  = \uri -> load uri,
        _onNewWindow      = \uri -> spawn "hbro" [show uri],
        _onResourceOpened = defaultResourceOpened,
        _onTitleChanged   = \title -> readGUI mainWindow >>= io . (`G.set` [ windowTitle G.:= ("hbro | " ++ title)])} -- return ()}

-- | List of default supported requests.
instance Default (CommandsMap K) where
    def = CommandsMap . M.fromList $ [
    -- Get information
        ("GET_URI",           \_arguments -> show <$> getURI),
        ("GET_TITLE",         \_arguments -> show <$> getTitle),
        ("GET_FAVICON_URI",   \_arguments -> show <$> getFaviconURI),
        ("GET_LOAD_PROGRESS", \_arguments -> show <$> getLoadProgress),
    -- Trigger actions
        ("LOAD_URI",          \arguments -> case arguments of
            uri:_ -> parseURIReference uri >>= load >> return "OK"
            _     -> return "ERROR Argument needed."),
        ("STOP_LOADING",      \_arguments -> stopLoading >> return "OK"),
        ("RELOAD",            \_arguments -> reload      >> return "OK"),
        ("GO_BACK",           \_arguments -> goBack      >> return "OK"),
        ("GO_FORWARD",        \_arguments -> goForward   >> return "OK"),
        ("ZOOM_IN",           \_arguments -> zoomIn      >> return "OK"),
        ("ZOOM_OUT",          \_arguments -> zoomOut     >> return "OK")]


defaultDownload :: URI -> String -> Int -> K ()
defaultDownload _ _ _ = return ()

defaultLinkClicked :: (MonadBase IO m, MonadWriter String m, GUIReader n m) => MouseButton -> URI -> m ()
defaultLinkClicked MiddleButton uri = spawn "hbro" [show uri]
defaultLinkClicked _            uri = load uri


defaultKeyBindings :: M.Map Keys.Mode (Keys.Bindings K)
defaultKeyBindings = M.singleton Keys.Normal $ Keys.toBindings [
    -- Browse
        ("M-<Left>",      goBack),
        ("M-<Right>",     goForward),
        ("C-<Escape>",    stopLoading),
        ("<F5>",          reload),
        ("C-r",           reload),
        ("C-<F5>",        reloadBypassCache),
        ("M-r",           reloadBypassCache),
        ("C-^",           scroll Horizontal (Absolute 0)),
        ("C-$",           scroll Horizontal (Absolute 100)),
        ("C-<Home>",      scroll Vertical   (Absolute 0)),
        ("C-<End>",       scroll Vertical   (Absolute 100)),
        ("M-<Home>",      goHome),
    -- Copy/paste
        ("C-c",           getURI   >>= Clipboard.insert . show >> notify 5000 "URI copied to clipboard"),
        ("M-c",           getTitle >>= Clipboard.insert >> notify 5000 "Page title copied to clipboard"),
        ("C-v",           Clipboard.with $ parseURIReference >=> load),
        ("M-v",           Clipboard.with $ \uri -> spawn "hbro" [uri]),
    -- Display
        ("C-+",           zoomIn),
        ("C--",           zoomOut),
        -- ("<F11>",         with (_window . _UI) windowFullscreen),
        -- ("<Escape>",      with (_window . _UI) windowUnfullscreen),
        ("C-b",           toggleVisibility =<< readGUI statusBar),
        ("C-u",           toggleSourceMode),
    -- Prompt
        ("C-o",           Prompt.readURI "Open URI" "" load),
        ("M-o",           getURI >>= \uri -> Prompt.readURI "Open URI " (show uri) load),
    -- Search
        ("/",             Prompt.iread "Search " "" $ searchText_ CaseInsensitive Forward Wrap),
        ("C-f",           Prompt.iread "Search " "" $ searchText_ CaseInsensitive Forward Wrap),
        ("?",             Prompt.iread "Search " "" $ searchText_ CaseInsensitive Backward Wrap),
        ("C-n",           void . searchText CaseInsensitive Forward  Wrap =<< Prompt.getEntryValue),
        ("C-N",           void . searchText CaseInsensitive Backward Wrap =<< Prompt.getEntryValue),
    -- Misc
        ("<Escape>",      Prompt.hide),
        ("C-i",           inspect),
        ("C-p",           printPage),
        ("C-t",           spawn "hbro" []),
        ("C-w",           quit)]

-- /!\ NetworkRequest's Haskell binding is missing the function "webkit_network_request_get_message", which makes it rather useless...
-- | Display content if webview can show the given MIME type, otherwise download it.
defaultResourceOpened :: (MonadBase IO m, GUIReader n m) => URI -> String -> m ResourceAction
defaultResourceOpened _uri mimetype = do
    canShow <- io . (`webViewCanShowMimeType` mimetype) =<< readGUI webView
    return (canShow ? Load ?? Download)
-- }}}

-- {{{ Util
isCaseSensitive :: CaseSensitivity -> Bool
isCaseSensitive CaseSensitive = True
isCaseSensitive _             = False

isForward :: Direction -> Bool
isForward Forward = True
isForward _       = False

isWrapped :: Wrap -> Bool
isWrapped Wrap = True
isWrapped _    = False


{-getState :: (MonadBase IO m, MonadError HError m, Typeable a) => String -> a -> m a
getState key defaultValue = do
    customMap <- gets _custom
    let result = fromDynamic =<< M.lookup key customMap
    case result of
        Just value -> return value
        _          -> do
            modify $ \s -> s { _custom = M.insert key (toDyn defaultValue) customMap }
            return defaultValue-}
-- }}}

-- {{{ Getters
getFaviconURI :: (MonadBase IO m, GUIReader n m, MonadError HError m) => m URI
getFaviconURI = W.getIconUri =<< readGUI webView

getLoadProgress :: (MonadBase IO m, GUIReader n m) => m Double
getLoadProgress = io . W.webViewGetProgress =<< readGUI webView

getURI :: (MonadBase IO m, GUIReader n m, MonadError HError m) => m URI
getURI = W.getUri =<< readGUI webView

getTitle :: (MonadBase IO m, GUIReader n m, MonadError HError m) => m String
getTitle = W.getTitle =<< readGUI webView
-- }}}


-- {{{ Browsing
goHome :: (MonadBase IO m, GUIReader n m, ConfigReader n' m, MonadWriter String m) => m ()
goHome = load =<< readConfig homePage

load :: (MonadBase IO m, GUIReader n m, MonadWriter String m) => URI -> m ()
load uri = do
    tell $ "Loading URI: " ++ (show uri')
    io . (`W.webViewLoadUri` uri') =<< readGUI webView
  where
    uri' = case uriScheme uri of
             [] -> "http://" ++ show uri
             _  -> show uri

reload, reloadBypassCache :: (MonadBase IO m, GUIReader n m, MonadError HError m) => m ()
reload            = io . W.webViewReload =<< readGUI webView
reloadBypassCache = io . W.webViewReloadBypassCache =<< readGUI webView

stopLoading :: (MonadBase IO m, GUIReader n m, MonadWriter String m) => m ()
stopLoading = do
    io . W.webViewStopLoading =<< readGUI webView
    tell $ "Stopped loading"

goBack, goForward :: (MonadBase IO m, GUIReader n m, MonadError HError m) => m ()
goBack = do
    readGUI webView >>= io . W.webViewCanGoBack >>= (`unless` throwError CannotGoBack)
    io . W.webViewGoBack =<< readGUI webView
goForward = do
    readGUI webView >>= io . W.webViewCanGoForward >>= (`unless` throwError CannotGoForward)
    readGUI webView >>= io . W.webViewGoForward
-- }}}

-- {{{ Display
-- | Toggle source display.
-- Current implementation forces a refresh of current web page, which may be undesired.
toggleSourceMode :: (MonadBase IO m, GUIReader n m, MonadError HError m) => m ()
toggleSourceMode = do
    v <- readGUI webView
    io . W.webViewSetViewSourceMode v =<< (io $ not <$> W.webViewGetViewSourceMode v)
    reload

zoomIn, zoomOut :: (MonadBase IO m, GUIReader n m) => m ()
zoomIn  = io . W.webViewZoomIn  =<< readGUI webView
zoomOut = io . W.webViewZoomOut =<< readGUI webView

scroll :: (MonadBase IO m, GUIReader n m) => Axis -> Position -> m ()
scroll axis percentage = SW.scroll axis percentage =<< readGUI scrollWindow


-- | Show web inspector for current webpage.
inspect :: (MonadBase IO m, GUIReader n m) => m ()
inspect = do
    inspector <- io . W.webViewGetInspector =<< readGUI webView
    io $ webInspectorInspectCoordinates inspector 0 0
-- }}}

-- {{{
searchText :: (MonadBase IO m, GUIReader n m) => CaseSensitivity -> Direction -> Wrap -> String -> m Bool
searchText s d w text = do
    v <- readGUI webView
    io $ W.webViewSearchText v text (isCaseSensitive s) (isForward d) (isWrapped w)

searchText_ :: (MonadBase IO m, GUIReader n m) => CaseSensitivity -> Direction -> Wrap -> String -> m ()
searchText_ s d w text = searchText s d w text >> return ()

printPage :: (MonadBase IO m, GUIReader n m) => m ()
printPage = io . W.webFramePrint =<< io . W.webViewGetMainFrame =<< readGUI webView

download :: (MonadBase IO m, ConfigReader m m) => URI -> m ()
download uri = do
    callback <- readConfig onDownload
    callback uri (show uri) 0
-- }}}

quit :: (MonadBase IO m) => m ()
quit = io mainQuit


-- {{{ Misc
-- | Execute a javascript file on current webpage.
executeJSFile :: (MonadBase IO m, MonadReader r m, MonadWriter String m) => FilePath -> WebView -> m ()
executeJSFile filePath webView' = do
    tell $ "Executing Javascript file: " ++ filePath
    script <- io $ readFile filePath
    let script' = unwords . map (++ "\n") . lines $ script

    io $ webViewExecuteScript webView' script'
-- }}}

-- | Save current web page to a file,
-- along with all its resources in a separated directory.
-- Doesn't work for now, because web_resource_get_data's binding is missing...
_savePage :: String -> WebView -> IO ()
_savePage _path webView' = do
    frame        <- webViewGetMainFrame webView'
    dataSource   <- webFrameGetDataSource frame
    _mainResource <- webDataSourceGetMainResource dataSource
    _subResources <- webDataSourceGetSubresources dataSource
    return ()
