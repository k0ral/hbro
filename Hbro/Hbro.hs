{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoRec #-}
module Hbro.Hbro (
-- * Main
    launchHbro
) where

-- {{{ Imports
--import Hbro.Config
import Hbro.Core
import Hbro.Gui
import Hbro.Keys
import Hbro.Socket
import Hbro.Types
import Hbro.Util

import qualified Config.Dyre as D
import Config.Dyre.Compile
import Config.Dyre.Paths

--import Control.Concurrent
import Control.Monad.Reader hiding(forM_, mapM_)

import Data.Dynamic
import Data.Foldable
import Data.IORef
import qualified Data.Map as M

import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.Entry.Editable
import Graphics.UI.Gtk.Entry.Entry
import Graphics.UI.Gtk.General.General hiding(initGUI)
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.WebKit.Download
import Graphics.UI.Gtk.WebKit.NetworkRequest
import Graphics.UI.Gtk.WebKit.WebFrame
import Graphics.UI.Gtk.WebKit.WebNavigationAction
import Graphics.UI.Gtk.WebKit.WebPolicyDecision
import Graphics.UI.Gtk.WebKit.WebView

import Network.URI

import Prelude hiding(concat, mapM_)

import System.Console.CmdArgs
import System.Directory
import System.Exit
import System.FilePath
import System.Glib.Signals
import System.IO
--import System.Posix.Process
import System.Posix.Signals
import qualified System.ZMQ as ZMQ
-- }}}

-- {{{ Commandline options
cliOptions :: CliOptions
cliOptions = CliOptions {
    mURI           = def &= help "URI to open at start-up" &= explicit &= name "u" &= name "uri" &= typ "URI",
    mVanilla       = def &= help "Do not read custom configuration file." &= explicit &= name "1" &= name "vanilla",
    mRecompile     = def &= help "Force recompilation and do not launch browser." &= explicit &= name "r" &= name "recompile",
    mDenyReconf    = def &= help "Deny recompilation even if the configuration file has changed." &= explicit &= name "deny-reconf",
    mForceReconf   = def &= help "Force recompilation even if the configuration file hasn't changed." &= explicit &= name "force-reconf",
    mDyreDebug     = def &= help "Force the application to use './cache/' as the cache directory, and ./ as the configuration directory. Useful to debug the program without installation." &= explicit &= name "dyre-debug",
    mMasterBinary  = def &= explicit &= name "dyre-master-binary"
}

getOptions :: IO CliOptions
getOptions = cmdArgs $ cliOptions
    &= verbosityArgs [explicit, name "verbose", name "v"] []
    &= versionArg [ignore]
    &= help "A minimal KISS-compliant browser."
    &= helpArg [explicit, name "help", name "h"]
    &= program "hbro"
-- }}}

-- {{{ Util
printDyrePaths :: IO ()
printDyrePaths = getPaths dyreParameters >>= \(a,b,c,d,e) -> (putStrLn . unlines) [
    "Current binary:  " ++ a,
    "Custom binary:   " ++ b,
    "Config file:     " ++ c,
    "Cache directory: " ++ d,
    "Lib directory:   " ++ e, []]

-- | Launch a recompilation of the configuration file
recompile :: IO (Maybe String)
recompile = do
    customCompile dyreParameters 
    getErrorString dyreParameters 

showError :: (Config, a) -> String -> (Config, a)
showError (config, x) message = (config { mError = Just message }, x)
-- }}}

-- {{{ Entry point      
dyreParameters :: D.Params (Config, CliOptions)
dyreParameters = D.defaultParams {
    D.projectName  = "hbro",
    D.showError    = showError,
    D.realMain     = realMain,
    D.ghcOpts      = ["-threaded"],
    D.statusOut    = hPutStrLn stderr
}

-- | Browser's main function.
-- To be called in main function with a proper configuration.
-- See Hbro.Main for an example.
launchHbro :: Config -> IO ()
launchHbro config = do
    options <- getOptions
-- Handle recompilation
    when (mRecompile options) $
        recompile
        >>= maybe exitSuccess (\e -> putStrLn e >> exitFailure)
-- Handle vanilla mode
    case mVanilla options of
        True -> D.wrapMain dyreParameters{ D.configCheck = False } (config, options)
        _    -> D.wrapMain dyreParameters                          (config, options)

-- At this point, the reconfiguration process is done
realMain :: (Config, CliOptions) -> IO ()
realMain (config, options) = do
-- Handle SIGINT
    void $ installHandler sigINT (Catch interruptHandler) Nothing
-- Print configuration state
    mapM_ putStrLn (mError config)
    whenLoud printDyrePaths
-- Initialize GUI, state and IPC socket
    gui   <- initGUI (mUIFile config) (mWebSettings config)
    state <- newIORef (M.empty :: M.Map String Dynamic)
    
    ZMQ.withContext 1 $ \context -> realMain' (Environment state options config gui context)
    whenNormal . putStrLn $ "Exiting..."

realMain' :: Environment -> IO ()
realMain' environment@Environment{ mOptions = options, mConfig = config, mGUI = gui} = let
    entry   = (mEntry . mPromptBar) gui
    webView = mWebView gui
    hooks   = mHooks config
  in do    
-- Bind hooks
    void $ on webView titleChanged                      (onTitleChanged environment)
    void $ on webView loadFinished                      (\_frame -> runK environment $ mLoadFinished hooks)
    void $ on webView navigationPolicyDecisionRequested (onNavigationRequest environment)
    void $ on webView newWindowPolicyDecisionRequested  (onNewWindow environment)
    void $ on webView createWebView                     (onNewWebView environment)
    void $ on webView downloadRequested                 (onDownload environment)
    void $ on webView mimeTypePolicyDecisionRequested   (onMIMEDisposition environment)
    
    void $ after webView keyPressEvent                  (onKeyPressed environment)
    void $ on entry   keyPressEvent                     (onPromptKeyPress environment)
    void $ on entry   editableChanged                   (onPromptChanged environment)        

-- Set start-up page
    startURI <- case (mURI options) of
        Just uri -> do 
            fileURI <- doesFileExist uri
            case fileURI of
                True -> getCurrentDirectory >>= \dir -> return $ parseURIReference ("file://" ++ dir </> uri)
                _    -> return $ parseURIReference uri
        _ -> return Nothing
    
    runK environment $ do
        openIPCSocket

    -- Custom start-up
        mStartUp . mHooks $ config
    -- Load home page        
        maybe goHome loadURI startURI
    -- Main loop
        io mainGUI
        
        closeIPCSocket

interruptHandler :: IO ()
interruptHandler = whenLoud (putStrLn "Received SIGINT.") >> mainQuit
-- }}}

-- {{{ Hooks
onDownload :: Environment -> Download -> IO Bool
onDownload environment download = do
    uri      <- (>>= parseURI) `fmap` downloadGetUri download
    filename <- downloadGetSuggestedFilename download
    size     <- downloadGetTotalSize download
    
    case (uri, filename) of
        (Just uri', Just filename') -> do
            whenNormal . putStrLn . ("Requested download: " ++) . show $ uri'
            runK environment $ callback uri' filename' size
        _ -> return ()
    return False
  where 
    callback = mDownload . mHooks . mConfig $ environment

onKeyPressed :: Environment -> EventM EKey Bool
onKeyPressed env = do
    modifiers <- eventModifier
    key'      <- keyToString `fmap` eventKeyVal

    io . forM_ key' $ \key -> do 
        let keystrokes = (++ key) . concat . map stringify $ modifiers
        runK env $ (mKeyPressed hooks) keystrokes
    return False
  where
    hooks = mHooks . mConfig $ env

onMIMEDisposition :: Environment -> WebFrame -> NetworkRequest -> String -> WebPolicyDecision -> IO Bool
onMIMEDisposition env _frame request mimetype decision = do
    uri <- (>>= parseURIReference) `fmap` networkRequestGetUri request
    forM_ uri (\u -> runK env $ hook u mimetype decision)
    return True
  where
    hook = mMIMEDisposition . mHooks . mConfig $ env

onNavigationRequest :: Environment -> WebFrame -> NetworkRequest -> WebNavigationAction -> WebPolicyDecision -> IO Bool
onNavigationRequest environment _frame request action decision = do
    uri    <- (>>= parseURIReference) `fmap` networkRequestGetUri request
    reason <- webNavigationActionGetReason action
    button <- webNavigationActionGetButton action

    let behavior = case (reason, button, uri) of
          (WebNavigationReasonLinkClicked,     1, Just u)  -> (mLinkClicked     hooks) ButtonL u decision
          (WebNavigationReasonLinkClicked,     2, Just u)  -> (mLinkClicked     hooks) ButtonM u decision
          (WebNavigationReasonLinkClicked,     3, Just u)  -> (mLinkClicked     hooks) ButtonR u decision
          (WebNavigationReasonFormSubmitted,   _, Just u)  -> (mFormSubmitted   hooks) u decision
          (WebNavigationReasonBackForward,     _, Just u)  -> (mBackForward     hooks) u decision
          (WebNavigationReasonReload,          _, Just u)  -> (mReload          hooks) u decision
          (WebNavigationReasonFormResubmitted, _, Just u)  -> (mFormResubmitted hooks) u decision
          (WebNavigationReasonOther,           _, Just u)  -> (mOtherNavigation hooks) u decision
          
          (WebNavigationReasonLinkClicked,     x, Just _)  -> io . whenNormal . putStrLn . ("WARNING: link clicked with invalid button: " ++) . show $ x
          _                                                -> io . whenNormal . putStrLn $ "WARNING: invalid link clicked."
    
    runK environment behavior
    return True
  where
    hooks = (mHooks . mConfig) environment

onNewWindow :: Environment -> WebFrame -> NetworkRequest -> WebNavigationAction -> WebPolicyDecision -> IO Bool
onNewWindow _env _frame request _action decision = do    
    uri    <- networkRequestGetUri request
    --reason <- webNavigationActionGetReason action
    --button <- webNavigationActionGetButton action 
              
    case uri of
       Just u -> (putStrLn . ("New window request: " ++) $ u) >> spawn "hbro" ["-u", u]
       _      -> putStrLn "WARNING: wrong URI given, unable to open new window."

    webPolicyDecisionIgnore decision
    
    return True
      
    

-- Triggered in 2 cases:
--  1/ Javascript window.open()
--  2/ Context menu  "Open in new window"
onNewWebView :: Environment -> WebFrame -> IO WebView
onNewWebView _env _frame = do
    --forM_ uri $ (runK env) . callback
    
    webView <- webViewNew
    
    void . on webView webViewReady $ return True        
    void . on webView navigationPolicyDecisionRequested $ \_ request _ decision -> do
        networkRequestGetUri request >>= mapM_ (\u -> spawn "hbro" ["-u", u])
        webPolicyDecisionIgnore decision
        return True
    
    return webView
  --where
    --callback = mNewWindow . mHooks . mConfig $ environment
    
-- Validate/cancel prompt
onPromptKeyPress :: Environment -> EventM EKey Bool
onPromptKeyPress env = do
    key <- eventKeyName
    io $ do  
      callback <- readIORef callbackRef

      when (key == "Return") . runK env $ io (entryGetText entry) >>= callback
      when (key == "Return" || key == "Escape") $ do        
          widgetHide box
          writeIORef callbackRef            (const $ return ())
          writeIORef incrementalCallbackRef (const $ return ())
          widgetGrabFocus webView
    return False
  where
    callbackRef            = mCallbackRef            . mPromptBar . mGUI $ env
    incrementalCallbackRef = mIncrementalCallbackRef . mPromptBar . mGUI $ env 
    entry                  = mEntry                  . mPromptBar . mGUI $ env
    box                    = mBox                    . mPromptBar . mGUI $ env
    webView                = mWebView                             . mGUI $ env
    
-- Incremental behavior
onPromptChanged :: Environment -> IO ()
onPromptChanged env = do
    callback <- readIORef incrementalCallbackRef
    runK env $ io (entryGetText entry) >>= callback
  where
    incrementalCallbackRef = mIncrementalCallbackRef . mPromptBar . mGUI $ env 
    entry                  = mEntry                  . mPromptBar . mGUI $ env

-- 
onTitleChanged :: Environment -> WebFrame -> String -> IO ()
onTitleChanged env _frame title = do
    whenLoud . putStrLn . ("Title changed: " ++) $ title
    runK env $ hook title
  where
    hook = mTitleChanged . mHooks . mConfig $ env
-- }}}  
