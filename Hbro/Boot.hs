module Hbro.Boot (hbro) where

-- {{{ Imports
import Hbro.Config
import Hbro.Core
import qualified Hbro.Dyre as Dyre
import Hbro.Error
import Hbro.Gui (GUI, GUIReader(..))
import qualified Hbro.Gui as Gui
import Hbro.IPC (IPC(..), IPCReader(..))
import qualified Hbro.IPC as IPC
import qualified Hbro.Keys as Key
import Hbro.Notification
import Hbro.Options (CliOptions)
import qualified Hbro.Options as Options
import Hbro.Util
import qualified Hbro.Webkit.WebSettings as WS
import Hbro.Webkit.WebView

-- import Control.Applicative
import Control.Concurrent
import Control.Conditional hiding(when, unless)
import Control.Lens hiding((??))
import Control.Monad
import Control.Monad.Base
import Control.Monad.Error hiding(when)
import Control.Monad.Trans.Control

import Data.Default
import Data.Functor
import Data.List
import qualified Data.Map as M hiding(null)
-- import Data.Maybe
import Data.Version

import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.Gdk.EventM
import qualified Graphics.UI.Gtk.General.General as GTK
import qualified Graphics.UI.Gtk.WebKit.Download as W
import Graphics.UI.Gtk.WebKit.WebSettings
import Graphics.UI.Gtk.WebKit.WebNavigationAction
import Graphics.UI.Gtk.WebKit.WebPolicyDecision
import Graphics.UI.Gtk.WebKit.WebView as W

import Paths_hbro

import Prelude hiding(init)

import System.Directory
import System.Environment.XDG.BaseDir
import System.Exit
import System.FilePath
import System.Glib.Signals
import System.Posix.Files
import System.Posix.Process
import System.Posix.Signals
import System.ZMQ3 (Rep(..))
import qualified System.ZMQ3 as ZMQ
-- }}}


-- | Main function to call in the configuration file (cf file @Hbro/Main.hs@).
-- First, commandline options are parsed, then configuration is dynamically applied.
hbro :: K () -> IO ()
hbro setup = do
    opts <- Options.get

    when (opts^.Options.help)      $ putStrLn Options.usage >> exitSuccess
    when (opts^.Options.version)   $ putStrLn (showVersion version) >> exitSuccess
    when (opts^.Options.verbose)   . putStrLn $ "Commandline options: " ++ show opts
    when (opts^.Options.recompile) $ Dyre.recompile >>= maybe exitSuccess (\e -> putStrLn e >> exitFailure)

    Dyre.wrap hbro' opts (setup, opts)


hbro' :: (K (), CliOptions) -> IO ()
hbro' (customSetup, options) = do
    config <- initConfig options
    gui    <- initGUI
    ipc    <- initIPC

    void $ installHandler sigINT (Catch onInterrupt) Nothing
    (result, logs) <- runK options config gui ipc $ main customSetup

    either print return result
    unless (options^.Options.quiet) . unless (null logs) $ putStrLn logs
    unless (options^.Options.quiet) $ putStrLn "Exiting..."

-- {{{ Initialization
initConfig :: (MonadBase IO m) => CliOptions -> m (Config K)
initConfig options = do
    let config = id
          . (options^.Options.quiet   ? set verbosity Quiet   ?? id)
          . (options^.Options.verbose ? set verbosity Verbose ?? id)
          $ def
    return config


initGUI :: (MonadBase IO m) => m (GUI K)
initGUI = do
    file     <- io (getUserConfigDir "hbro" >/> "ui.xml")
    fallback <- io $ getDataFileName "examples/ui.xml"

    file' <- io $ firstReadableOf [file, fallback]

    case file' of
        Just f -> do
          io $ void GTK.initGUI
          Gui.buildFrom f
        _ -> io $ putStrLn "No UI file found." >> exitFailure
  where
    firstReadableOf []    = return Nothing
    firstReadableOf (x:y) = do
        isReadable <- fileAccess x True False False
        isReadable ? return (Just x) ?? firstReadableOf y


initIPC :: (MonadBase IO m) => m IPC
initIPC = io $ do
    theContext <- ZMQ.init 1
    socket     <- ZMQ.socket theContext Rep
    ZMQ.bind socket =<< getSocketURI
    return $ IPC theContext socket


getSocketURI :: (MonadBase IO m) => m String
getSocketURI = do
    dir <- io getTemporaryDirectory
    pid <- io getProcessID
    return $ "ipc://" ++ dir </> "hbro." ++ show pid
-- }}}


main :: K () -> K ()
main customSetup = do
    threadSync <- fork socketMain

    Gui.init

-- Bind hooks
    bindDownload          =<< Gui.readGUI Gui.webView
    bindKeys
    bindLoadFinished      =<< Gui.readGUI Gui.webView
    bindNavigationRequest =<< Gui.readGUI Gui.webView
    bindNewWebView        =<< Gui.readGUI Gui.webView
    bindNewWindow         =<< Gui.readGUI Gui.webView
    bindResourceOpened    =<< Gui.readGUI Gui.webView
    bindTitleChanged      =<< Gui.readGUI Gui.webView

-- Default web settings
    WS.set webSettingsMonospaceFontFamily               "consolas"
    WS.set webSettingsEnableDeveloperExtras             True
    WS.set webSettingsEnableHtml5Database               False
    WS.set webSettingsEnableHtml5LocalStorage           False
    WS.set webSettingsEnablePageCache                   True
    WS.set webSettingsEnablePlugins                     False
    WS.set webSettingsEnablePrivateBrowsing             False
    WS.set webSettingsEnableScripts                     False
    WS.set webSettingsEnableSpellChecking               False
    WS.set webSettingsEnableSpatialNavigation           True
    WS.set webSettingsEnableUniversalAccessFromFileUris True
    WS.set webSettingsEnableXssAuditor                  True
    WS.set webSettingsJSCanOpenWindowAuto               False

-- Apply custom setup
    customSetup

    config <- readConfig id
    logV $ "Start-up configuration: \n" ++ show config

-- Load startpage
    startURI <- Options.getStartURI
    maybe goHome load startURI

-- Main loop
    io GTK.mainGUI

-- Clean & close
    void . (`IPC.sendCommand` "QUIT") =<< getSocketURI
    io $ takeMVar threadSync

    io . ZMQ.close =<< readIPC IPC.receiver
    io . ZMQ.term =<< readIPC IPC.context


-- | IPC thread that listens to commands from external world.
socketMain :: K ()
socketMain = do
    socket <- readIPC IPC.receiver

    whileTrue $ do
        message <- IPC.read socket
        logV $ "Received command: " ++ message

        case words message of
            []                -> IPC.send socket "ERROR Empty command" >> return True
            "QUIT":[]         -> IPC.send socket "OK" >> return False
            command:arguments -> do
                commands' <- IPC.unwrap <$> readConfig commands
                case M.lookup command commands' of
                    Just callback -> (postGUISync' (callback arguments) >>= IPC.send socket) `catchError` (\_ -> IPC.send socket "ERROR")
                    _             -> IPC.send socket "ERROR Unknown command"
                return True
    return ()
  where
    whileTrue f = do
        result <- f
        result ? whileTrue f ?? return ()


onInterrupt :: (MonadBase IO m) => m ()
onInterrupt = io (putStrLn "Received SIGINT." >> GTK.mainQuit)


-- {{{ Signals handlers
-- Triggered in 2 cases:
--  1/ Javascript window.open()
--  2/ Context menu "Open in new window"
bindNewWebView :: (MonadBase IO m, ConfigReader m m, MonadBaseControl IO m, MonadError HError m) => WebView -> m ()
bindNewWebView webView = do
    void . liftBaseWith $ \runInIO -> on webView W.createWebView $ \_frame -> do
        webView' <- webViewNew

        void . on webView' W.webViewReady $ return True
        void . on webView' W.navigationPolicyDecisionRequested $ \_ request _ decision -> do
            void . runInIO $ do
                callback <- readConfig onNewWindow
                uri      <- networkRequestGetUri request
                logV $ "New webview <" ++ show uri ++ ">"
                callback uri
            webPolicyDecisionIgnore decision
            return True

        return webView'


bindDownload :: (MonadBase IO m, MonadBaseControl IO m, ConfigReader m m, MonadError HError m) =>  WebView -> m ()
bindDownload webView = do
    void . liftBaseWith $ \runInIO -> on webView W.downloadRequested $ \d -> do
        amount <- W.downloadGetTotalSize d

        --notify 5000 $ "Requested download: " ++ filename ++ " (" ++ show size ++ ")"
        void . runInIO $ do
            uri      <- downloadGetUri d
            filename <- downloadGetSuggestedFilename d
            callback <- readConfig onDownload
            logV $ "Requested download <" ++ show uri ++ ">"
            callback uri filename amount
        return False


bindLoadFinished :: (MonadBase IO m, MonadBaseControl IO m, ConfigReader m m, Error e, Show e, MonadError e m) => WebView -> m ()
bindLoadFinished webView = do
    void . liftBaseWith $ \runInIO -> on webView W.loadFinished $ \_frame-> do
        void . runInIO $ do
            callback <- readConfig onLoadFinished
            logV "Load finished"
            callback `catchError` \e -> (io $ print e) -- >> notify 5000 (show e)
        return ()

bindNavigationRequest :: (MonadBase IO m, MonadBaseControl IO m, ConfigReader m m, MonadError HError m) => WebView -> m ()
bindNavigationRequest webView = do
    liftBaseWith $ \runInIO -> void . on webView W.navigationPolicyDecisionRequested $ \_frame request action decision -> do
        reason <- io $ webNavigationActionGetReason action
        button <- io $ toMouseButton <$> webNavigationActionGetButton action

        case (reason, button) of
            (WebNavigationReasonLinkClicked, Just b) -> void . runInIO $ do
                callback <- readConfig onLinkClicked
                uri      <- networkRequestGetUri request
                logV $ "Link clicked <" ++ show uri ++ ">"
                callback b uri
                io $ webPolicyDecisionIgnore decision
            _ -> {-void . runInIO $ do
                callback <- readConfig onLoadRequested
                uri      <- networkRequestGetUri request
                logV $ "Requested load <" ++ show uri ++ ">"
                callback uri-}
                io $ webPolicyDecisionUse decision
        return True
  where
    toMouseButton 1 = Just LeftButton
    toMouseButton 2 = Just MiddleButton
    toMouseButton 3 = Just RightButton
    toMouseButton _ = Nothing


bindNewWindow :: (MonadBase IO m, MonadBaseControl IO m, ConfigReader m m, MonadError HError m) => WebView -> m ()
bindNewWindow webView = do
    liftBaseWith $ \runInIO -> void . on webView W.newWindowPolicyDecisionRequested $ \_frame request _action decision -> do
        void . runInIO $ do
            callback <- readConfig onNewWindow
            uri      <- networkRequestGetUri request
            logV $ "New window request <" ++ show uri ++ ">"
            callback uri
        webPolicyDecisionIgnore decision
        return True
        --either (\e -> io . putStrLn $ "WARNING: wrong URI given, unable to open new window.") (const $ return ()) result


bindResourceOpened :: (MonadBase IO m, MonadBaseControl IO m, ConfigReader m m, MonadError HError m) => WebView -> m ()
bindResourceOpened webView = do
    liftBaseWith $ \runInIO -> void . on webView W.mimeTypePolicyDecisionRequested $ \_frame request mimetype decision -> do
        void . runInIO $ do
            callback <- readConfig onResourceOpened
            uri <- networkRequestGetUri request
            logV $ "Opening resource [MIME type=" ++ mimetype ++ " | uri=" ++ show uri ++ "]"
            action <- callback uri mimetype
            case action of
                Load -> io $ webPolicyDecisionUse      decision
                _    -> io $ webPolicyDecisionDownload decision
        return True


bindTitleChanged :: (MonadBase IO m, MonadBaseControl IO m, ConfigReader m m, MonadError HError m) => WebView -> m ()
bindTitleChanged webView = do
    liftBaseWith $ \runInIO -> void . on webView W.titleChanged $ \_frame title -> void . runInIO $ do
        logV $ "Title changed to: " ++ title
        callback <- readConfig onTitleChanged
        callback title


bindKeys :: (MonadBaseControl IO m, Key.StatusState m, ConfigReader m m, GUIReader m m, NotificationReader m, Error e, Show e, MonadError e m) => m ()
bindKeys = do
  wv <- readGUI Gui.webView
  liftBaseWith $ \runInIO -> void . on wv keyPressEvent $ do
    modifiers <- eventModifier
    keyVal    <- eventKeyVal

    case (Key.mkStroke modifiers keyVal) of
        Just newStroke -> void . io . runInIO $ do
            oldStrokes  <- Key.readStatus Key.strokes
            theMode     <- Key.readStatus Key.mode
            theBindings <- readConfig keyBindings
            f           <- readConfig onKeyStroke
            let allStrokes = oldStrokes ++ [newStroke]
            f allStrokes
            logV $ "Pressed keys: " ++ (intercalate  " " . map Key.serialize) allStrokes

            case (Key.lookup allStrokes =<< M.lookup theMode theBindings) of
                Just (Key.Leaf callback) -> Key.writeStatus Key.strokes [] >> callback `catchError` \e -> (io $ print e) >> notify 5000 (show e)
                Just (Key.Branch _)      -> Key.writeStatus Key.strokes allStrokes
                _                    -> return () --}
            return ()
        _ -> return ()
    return False
