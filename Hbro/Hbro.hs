{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoRec #-}
module Hbro.Hbro where

-- {{{ Imports
--import Hbro.Config
import Hbro.Core
import Hbro.Gui
import Hbro.Keys
import qualified Hbro.Prompt as Prompt
import qualified Hbro.Socket as Socket
import Hbro.Types
import Hbro.Util

--import Control.Concurrent
import Control.Monad.Reader hiding(forM_, mapM_)

import Data.Dynamic
import Data.Foldable
import Data.Functor
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
import System.FilePath
import System.Glib.Signals
--import System.Posix.Process
import qualified System.ZMQ as ZMQ
-- }}}


-- At this point, the reconfiguration process is done
main :: (Config', CliOptions) -> IO ()
main (Left e, _)             = putStrLn e
main (Right config, options) = do
-- Initialize GUI, state and IPC socket
    gui   <- initGUI (mUIFile config) (mWebSettings config)
    state <- newIORef (M.empty :: M.Map String Dynamic)
    
    ZMQ.withContext 1 $ \context -> main' (Environment state options config gui context)
    whenNormal . putStrLn $ "Exiting..."

main' :: Environment -> IO ()
main' environment@Environment{ mOptions = options, mConfig = config, mGUI = gui} = let
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
        Socket.open

    -- Custom start-up
        mStartUp . mHooks $ config
    -- Load home page        
        maybe goHome loadURI startURI
    -- Main loop
        io mainGUI
        
        Socket.close
-- }}}

-- {{{ Hooks
onDownload :: Environment -> Download -> IO Bool
onDownload environment download = do
    uri      <- fmap (>>= parseURI) . downloadGetUri $ download
    filename <- downloadGetSuggestedFilename download
    size     <- downloadGetTotalSize download
    
    case (uri, filename) of
        (Just uri', Just filename') -> do
            logVerbose . ("Requested download: " ++) . show $ uri'
            runK environment $ do
                notify 5000 $ "Requested download: " ++ filename' ++ " (" ++ show size ++ ")"
                callback uri' filename' size
        _ -> return ()
    return False
  where 
    callback = mDownload . mHooks . mConfig $ environment

onKeyPressed :: Environment -> EventM EKey Bool
onKeyPressed env = do
    modifiers <- eventModifier
    key'      <- keyToString <$> eventKeyVal

    io . forM_ key' $ \key -> do 
        let keystrokes = (++ key) . concat . map stringify $ modifiers
        logVerbose $ "Key pressed: " ++ keystrokes
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
          runK env Prompt.clean
          widgetGrabFocus webView
    return False
  where
    callbackRef            = mCallbackRef            . mPromptBar . mGUI $ env
    entry                  = mEntry                  . mPromptBar . mGUI $ env
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
    logVerbose $ "Title changed: " ++ title
    runK env $ (mTitleChanged . mHooks . mConfig $ env) title
-- }}}  
