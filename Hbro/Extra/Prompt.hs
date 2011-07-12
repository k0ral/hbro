module Hbro.Extra.Prompt where

-- {{{ Imports
import Hbro.Core
import Hbro.Gui
import Hbro.Types

import Graphics.UI.Gtk.Entry.Entry
import Graphics.UI.Gtk.WebKit.WebView
-- }}}


-- | Prompt for key words to search in current webpage.
promptFind :: Bool -> Bool -> Bool -> Browser -> IO ()
promptFind caseSensitive forward wrap browser =
    prompt "Search" "" True browser (\browser' -> do
        keyWord <- entryGetText (mPromptEntry $ mGUI browser')
        found   <- webViewSearchText (mWebView $ mGUI browser) keyWord caseSensitive forward wrap
        return ())

-- | Switch to next found key word.
findNext :: Bool -> Bool -> Bool -> Browser -> IO ()
findNext caseSensitive forward wrap browser = do
    keyWord <- entryGetText (mPromptEntry $ mGUI browser)
    found   <- webViewSearchText (mWebView $ mGUI browser) keyWord caseSensitive forward wrap 
    return ()

-- | Prompt for URI to open in current window.
promptURL :: Bool -> Browser -> IO()        
promptURL False browser = 
    prompt "Open URL" "" False browser (\b -> do 
        uri <- entryGetText (mPromptEntry $ mGUI b)
        loadURL uri b)
promptURL _ browser = do
    uri <- webViewGetUri (mWebView $ mGUI browser)
    case uri of
        Just url -> prompt "Open URL" url False browser (\b -> do
                        u <- entryGetText (mPromptEntry $ mGUI b)
                        loadURL u b)
        _ -> return ()
