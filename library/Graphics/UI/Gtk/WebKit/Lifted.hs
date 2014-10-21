-- | Some lifted functions from 'Graphics.UI.Gtk.WebKit'.
-- Designed to be imported as @qualified@.
module Graphics.UI.Gtk.WebKit.Lifted (
-- * 'NetworkRequest'
    networkRequestGetUri,
-- * 'Download'
    downloadGetUri,
-- * 'NetworkRequest'
    downloadGetSuggestedFilename,
) where

-- {{{ Imports
import           Hbro.Error
import           Hbro.Prelude

import qualified Graphics.UI.Gtk.WebKit.Download       as W
import           Graphics.UI.Gtk.WebKit.NetworkRequest (NetworkRequest)
import qualified Graphics.UI.Gtk.WebKit.NetworkRequest as W

import           Network.URI.Monadic
-- }}}

-- | Error message
unavailableFileName, unavailableURI :: Text
unavailableFileName = "No file name available"
unavailableURI = "No URI available"

networkRequestGetUri :: (MonadIO m, MonadError Text m) => NetworkRequest -> m URI
networkRequestGetUri r = parseURIReference =<< io (W.networkRequestGetUri r) <!> unavailableURI

downloadGetUri :: (MonadIO m, MonadError Text m) => W.Download -> m URI
downloadGetUri d = parseURI =<< io (W.downloadGetUri d) <!> unavailableURI

downloadGetSuggestedFilename :: (MonadIO m, MonadError Text m) => W.Download -> m Text
downloadGetSuggestedFilename d = io (W.downloadGetSuggestedFilename d) <!> unavailableFileName
