{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Hbro.Gui (
-- * Types
      GUI
    , HasGUI(..)
-- * Basic
    , get
    , initialize
-- * Getter
    , canRender
    , isSourceMode
    , getDOM
-- * Actions
    , toggle
    , render
    , scroll
    , setSourceMode
    , toggleSourceMode
    , zoomIn
    , zoomOut
-- * Misc
    , Axis(..)
    , Position(..)
    , getObject
) where

-- {{{ Imports
import Hbro.Error as Hbro
import Hbro.Gui.Buildable
import Hbro.Gui.NotificationBar (HasNotificationBar(..))
import qualified Hbro.Gui.NotificationBar as NotifBar
import Hbro.Gui.PromptBar (PromptBar, HasPromptBar(..))
import qualified Hbro.Gui.PromptBar as Prompt
import Hbro.Gui.PromptBar.Signals as Prompt
import Hbro.Gui.StatusBar
import Hbro.Util
import Hbro.Webkit.Lifted

-- import Control.Exception
import Control.Lens.Getter
import Control.Lens.Lens
import Control.Lens.Setter
import Control.Lens.TH
import Control.Monad.Reader hiding(join, mapM_, when)

import Data.Text as Text (intercalate, pack, snoc, splitOn, unpack)
import Data.Text.Lazy as Lazy (Text)

import Graphics.Rendering.Pango.Enums
import Graphics.UI.Gtk.Abstract.Container
import Graphics.UI.Gtk.Abstract.Widget
import qualified Graphics.UI.Gtk.Builder as Gtk
import Graphics.UI.Gtk.General.General as GTK
import Graphics.UI.Gtk.Misc.Adjustment
import Graphics.UI.Gtk.Scrolling.ScrolledWindow
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.DOM.Document
import Graphics.UI.Gtk.Windows.Window

import Network.URI as N

import Prelude hiding(mapM_, read)

import System.Glib.Attributes hiding(get, set)
import qualified System.Glib.Attributes as G (get, set)
-- import System.Glib.GError
import System.Glib.Signals
import System.Glib.Types
-- }}}

-- {{{ Types
data Axis     = Horizontal | Vertical deriving(Show)
data Position = Absolute Double | Relative Double deriving(Show)

data GUI = GUI
    { _mainWindow      :: Window
    , _scrollWindow    :: ScrolledWindow  -- ^ 'ScrolledWindow' containing the webview
    , _webView         :: WebView
    , _promptBar       :: PromptBar
    , _statusBar       :: StatusBar
    , _notificationBar :: NotifBar.NotificationBar
    , _builder         :: Gtk.Builder          -- ^ Builder object created from XML file
    }

makeLensesWith ?? ''GUI $ classyRules
    & lensField .~ (\name -> Just (tail name ++ "L"))
    & lensClass .~ (\name -> Just ("Has" ++ name, "_" ++ map toLower name))

-- | A 'GUI' can be built from an XML file.
instance Buildable GUI where
    buildWith b = do
        sWindow <- gSync $ Gtk.builderGetObject b castToScrolledWindow "webViewParent"
        webView <- gSync webViewNew

        gAsync $ containerAdd sWindow webView

        GUI <$> gSync (Gtk.builderGetObject b castToWindow "mainWindow")
            <*> pure sWindow
            <*> pure webView
            <*> buildWith b
            <*> buildWith b
            <*> buildWith b
            <*> pure b

instance HasNotificationBar GUI where _notificationbar = notificationBarL
instance HasPromptBar GUI       where _promptbar       = promptBarL
-- }}}

get :: (MonadReader r m, MonadBase IO m, HasGUI r) => Lens' GUI a -> m a
get l = askl (_gui.l)


-- {{{ Initialization
initialize :: (MonadBase IO m, MonadThrow m, MonadPlus m) => FilePath -> m GUI
initialize file = do
    gui <- buildFrom file

    let webView = gui^.webViewL

    initializeWindow    $ gui^.mainWindowL
    initializeWebView   webView
    Prompt.initialize   $ gui^.promptBarL
    NotifBar.initialize $ gui^.notificationBarL

    gAsync . widgetShowAll $ gui^.mainWindowL
    runReaderT Prompt.hide (gui^.promptBarL)

    gAsync $ windowSetDefault (gui^.mainWindowL) (Just $ gui^.webViewL)

    let closePrompt = widgetHide (gui^.promptBarL.boxL) >> widgetGrabFocus webView >> return ()
    onEntryCancelled (gui^.promptBarL.entryL) $ const closePrompt
    onEntryActivated (gui^.promptBarL.entryL) $ const closePrompt

    -- io $ scrolledWindowSetPolicy (gui^.scrollWindowL) PolicyNever PolicyNever
    -- io $ G.set (gui^.scrollWindowL) [ scrolledWindowHscrollbarPolicy := PolicyNever, scrolledWindowVscrollbarPolicy := PolicyNever]

    return gui

buildFrom :: (MonadBase IO m, MonadThrow m, Buildable t) => FilePath -> m t
buildFrom uiFile = do
    io . infoM "hbro.gui" $ "Building UI from: " ++ uiFile

    builder <- gSync Gtk.builderNew

    {-result <- -}
    gSync $ Gtk.builderAddFromFile builder uiFile
    -- leftM throwError result

    buildWith builder

-- TODO: catch IOException
-- builderAddFromFile ::
-- builderAddFromFile builder file = catchGErrorJustDomain (Right <$> Gtk.builderAddFromFile builder file) handler
--   where
--     handler :: Gtk.BuilderError -> String -> IO (Either String a)
--     handler e message = return . Left $ "Error while building GUI from [" ++ file ++ "]: " ++ message

initializeWindow :: (MonadBase IO m) => Window -> m ()
initializeWindow window = gAsync $ do
    widgetModifyBg window StateNormal (Color 0 0 5000)
    void . on window deleteEvent $ gAsync GTK.mainQuit >> return False

initializeWebView :: (MonadBase IO m) => WebView -> m ()
initializeWebView webView = gAsync $ do
    G.set webView [ widgetCanDefault := True ]
    -- webViewSetMaintainsBackForwardList webView False
    on webView closeWebView $ gAsync GTK.mainQuit >> return False
    void . on webView consoleMessage $ \a b n c -> do
        putStrLn "console message"
        mapM_ putStrLn [a, b, show n, c]
        return True
    -- void . on webView resourceRequestStarting $ \frame resource request response -> do
    --     uri <- webResourceGetUri resource
    --     putStrLn $ "resource request starting: " ++ uri
    --     -- print =<< webResourceGetData resource
    --     putStrLn =<< (maybe (return "No request") (return . ("Request URI: " ++) . show <=< W.networkRequestGetUri) request)
    --     putStrLn =<< (maybe (return "No response") (return . ("Response URI: " ++) . show <=< networkResponseGetUri) response)

    --     -- case (endswith ".css" uri || uri `endswith` ".png" || uri `endswith` ".ico") of
    --        -- True -> (putStrLn "OK")
    --     (maybe (return ()) (`networkRequestSetUri` "about:blank") request)
-- }}}

-- {{{ Actions
-- | Toggle a widget's visibility
toggle :: (MonadBase IO m, WidgetClass a) => a -> m ()
toggle widget = do
    visibility <- gSync $ G.get widget widgetVisible
    gAsync $ (widgetHide <| visibility |> widgetShow) widget

canRender :: (MonadBase IO m, MonadReader t m, HasGUI t) => String -> m Bool
canRender mimetype = gSync . (`webViewCanShowMimeType` mimetype) =<< get webViewL


render :: (MonadReader t m, HasGUI t, MonadBase IO m) => Lazy.Text -> URI -> m ()
render page uri = do
    io . debugM "hbro.gui" $ "Rendering <" ++ show uri ++ ">"
    -- loadString page uri =<< get' webViewL

    -- io . debugM "hbro.gui" $ "Base URI: " ++ show (baseOf uri)

    loadString page (baseOf uri) =<< get webViewL
  where
    baseOf uri' = uri' {
        uriPath = Text.unpack . (`snoc` '/') . Text.intercalate "/" . Prelude.init . splitOn "/" . Text.pack $ uriPath uri'
    }

scroll :: (MonadBase IO m, MonadReader t m, HasGUI t) => Axis -> Position -> m ()
scroll axis percentage = scroll' axis percentage =<< get scrollWindowL

-- General scrolling command
scroll' :: (MonadBase IO m) => Axis -> Position -> ScrolledWindow -> m ()
scroll' axis percentage scrollWindow = do
     logDebug $ "Set scroll " ++ show axis ++ " = " ++ show percentage

     adj     <- gSync . getAdjustment axis $ scrollWindow
     page    <- gSync $ adjustmentGetPageSize adj
     current <- gSync $ adjustmentGetValue adj
     lower   <- gSync $ adjustmentGetLower adj
     upper   <- gSync $ adjustmentGetUpper adj

     let shift (Absolute x) = lower   + x/100 * (upper - page - lower)
         shift (Relative x) = current + x/100 * page
         limit x            = (x `max` lower) `min` (upper - page)

     gAsync . adjustmentSetValue adj $ limit (shift percentage)


getAdjustment :: (MonadBase IO m) => Axis -> ScrolledWindow -> m Adjustment
getAdjustment Horizontal = gSync . scrolledWindowGetHAdjustment
getAdjustment Vertical   = gSync . scrolledWindowGetVAdjustment


-- TODO: see if the lens system can be leveraged to get/set this property
-- sourceMode :: Lens

isSourceMode :: (MonadBase IO m, MonadReader t m, HasGUI t) => m Bool
isSourceMode = gSync . webViewGetViewSourceMode =<< get webViewL

setSourceMode :: (MonadBase IO m, MonadReader t m, HasGUI t) => Bool -> m ()
setSourceMode value = get webViewL >>= gAsync . (`webViewSetViewSourceMode` value) >> logDebug ("Set source mode = " ++ show value)

-- | Toggle source display. This needs to be done *before* loading an URI.
toggleSourceMode :: (MonadBase IO m, MonadReader t m, HasGUI t) => m ()
toggleSourceMode = setSourceMode . not =<< isSourceMode


zoomIn, zoomOut :: (MonadBase IO m, MonadReader t m, HasGUI t) => m ()
zoomIn  = get webViewL >>= gAsync . webViewZoomIn >> logDebug "Zooming in."
zoomOut = get webViewL >>= gAsync . webViewZoomOut >> logDebug "Zooming out."

getDOM :: (MonadBase IO m, MonadReader t m, HasGUI t) => m (Maybe Document)
getDOM = gSync . webViewGetDomDocument =<< get webViewL

-- | Return the casted 'GObject' corresponding to the given name (set in the builder's XML file)
getObject :: (MonadBase IO m, MonadReader t m, HasGUI t, GObjectClass a)
          => (GObject -> a)   -- ^ @castTo@ function
          -> String           -- ^ Widget name
          -> m a
getObject cast name = do
    b <- get builderL
    gSync $ Gtk.builderGetObject b cast name
-- }}}


-- {{{ Util
logDebug{-, logInfo-} :: (MonadBase IO m) => String -> m ()
logDebug = io . debugM "hbro.gui"
-- logInfo  = io . infoM  "hbro.gui"
-- }}}
