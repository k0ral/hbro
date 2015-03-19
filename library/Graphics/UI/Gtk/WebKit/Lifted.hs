{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Some lifted functions from 'Graphics.UI.Gtk.WebKit'.
-- Designed to be imported as @qualified@.
module Graphics.UI.Gtk.WebKit.Lifted (
-- * 'NetworkRequest'
    networkRequestGetUri,
-- * 'Download'
    downloadGetUri,
-- * 'NetworkRequest'
    downloadGetSuggestedFilename,
-- * 'WebDataSource'
    dataSourceGetData
    ) where

-- {{{ Imports
import           Hbro.Error
import           Hbro.Prelude

import qualified Graphics.UI.Gtk.WebKit.Download       as W
import           Graphics.UI.Gtk.WebKit.NetworkRequest (NetworkRequest)
import qualified Graphics.UI.Gtk.WebKit.NetworkRequest as W
import           Graphics.UI.Gtk.WebKit.WebDataSource  as W

import           Network.URI.Extended
-- }}}

-- | Error message
unavailableData, unavailableFileName, unavailableURI :: Text
unavailableData = "No data available in the web frame."
unavailableFileName = "No file name available"
unavailableURI = "No URI available"

networkRequestGetUri :: (MonadIO m, MonadError Text m) => NetworkRequest -> m URI
networkRequestGetUri r = parseURIReferenceM =<< io (W.networkRequestGetUri r) <!> unavailableURI

downloadGetUri :: (MonadIO m, MonadError Text m) => W.Download -> m URI
downloadGetUri d = parseURIM =<< io (W.downloadGetUri d) <!> unavailableURI

downloadGetSuggestedFilename :: (MonadIO m, MonadError Text m) => W.Download -> m Text
downloadGetSuggestedFilename d = io (W.downloadGetSuggestedFilename d) <!> unavailableFileName

dataSourceGetData :: (MonadIO m, MonadError Text m) => WebDataSource -> m ByteString
dataSourceGetData ds = io (webDataSourceGetData ds) <!> unavailableData
