module Hbro.Error where

-- {{{ Imports
import Control.Monad.Error

import Data.Maybe

import Graphics.UI.Gtk.WebKit.Download
import Graphics.UI.Gtk.WebKit.NetworkRequest

import System.IO.Error
-- }}}

data HError =
    CannotGoBack
  | CannotGoForward
  | EmptyCallback
  | EmptyClipboard
  | EmptyDownloadURI Download
  | EmptyRequestURI NetworkRequest
  | EmptySuggestedFileName Download
  | InvalidIconURI
  | InvalidPageTitle
  | InvalidPageURI
  | InvalidURI String
  | IOE IOError
  | OtherError String

instance Error HError where
    strMsg = OtherError

instance Show HError where
    show CannotGoBack               = "Unable to go back: already at oldest page."
    show CannotGoForward            = "Unable to go forward: already at newest page."
    show (IOE e)                    = "IO error: " ++ ioeGetLocation e ++ ": " ++ fromMaybe "" (ioeGetFileName e) ++ " " ++ ioeGetErrorString e
    show InvalidIconURI             = "No favicon URI."
    show InvalidPageTitle           = "No page title."
    show InvalidPageURI             = "Invalid page URI."
    show (InvalidURI s)             = show s
    show (EmptyDownloadURI _)       = "Invalid download URI."
    show (EmptyClipboard)           = "Empty clipboard."
    show (EmptySuggestedFileName _) = "No suggested name for this download."
    show (EmptyRequestURI _)        = "Invalid request URI."
    show EmptyCallback              = "No callback defined."
    show (OtherError s)             = show s
