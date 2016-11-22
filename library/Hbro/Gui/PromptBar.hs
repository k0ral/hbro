{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Designed to be imported as @qualified@.
module Hbro.Gui.PromptBar (
-- * Types
      PromptBar
    , box_
    , closed_
    , buildFrom
    , labelName
    , entryName
    , boxName
    , PromptException(..)
-- * Functions
    , initialize
    , close
    , Hbro.Gui.PromptBar.clean
-- * Prompts
    , prompt
    , uriPrompt
    , iprompt
    , getPromptValue
-- * Monadic versions
    , promptM
    , uriPromptM
    , ipromptM
    , getPromptValueM
) where

-- {{{ Imports
import           Hbro.Error
import           Hbro.Event
import           Hbro.Gdk.KeyVal
import           Hbro.Gui.Builder
import           Hbro.Logger
import           Hbro.Prelude

import           Control.Concurrent.Async.Lifted
import           Control.Lens.Getter
import           Control.Lens.TH
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Resource hiding(throwM)

import           Graphics.Rendering.Pango.Extended
import           Graphics.UI.Gtk.Abstract.Widget
import qualified Graphics.UI.Gtk.Builder                  as Gtk
import           Graphics.UI.Gtk.Display.Label
import           Graphics.UI.Gtk.Entry.Editable
import           Graphics.UI.Gtk.Entry.Entry
import           Graphics.UI.Gtk.Gdk.EventM               as Gdk
import           Graphics.UI.Gtk.General.General.Extended
import           Graphics.UI.Gtk.Layout.HBox

import           Network.URI.Extended

import           System.Glib.Attributes.Extended
import           System.Glib.Signals                      hiding (Signal)
-- }}}

-- {{{ Types
data Closed = Closed deriving(Show)
instance Event Closed where
  describeInput _ _ = Just "Prompt closed."

data Changed = Changed deriving(Show)
instance Event Changed where
  type Input Changed = Text
  describeInput _ = Just . (<>) "Prompt value changed to: "

data Validated = Validated deriving(Show)
instance Event Validated where
  type Input Validated = Text
  describeInput _ = Just . (<>) "Prompt validated with value: "

-- | No exported constructor, please use 'buildFrom'
declareLenses [d|
  data PromptBar = PromptBar
    { box_         :: HBox
    , description_ :: Label
    , entry_       :: Entry
    , changed_     :: Signal Changed
    , closed_      :: Signal Closed
    , validated_   :: Signal Validated
    }
  |]

data PromptException = PromptInterrupted deriving(Eq, Show)

instance Exception PromptException where
  displayException PromptInterrupted = "Prompt interrupted."
-- }}}

-- | A 'PromptBar' can be built from an XML file.
buildFrom :: (ControlIO m, MonadLogger m) => Gtk.Builder -> m PromptBar
buildFrom builder = do
    entry        <- getWidget builder entryName
    closedSignal <- newSignal Closed
    validated    <- newSignal Validated

    promptBar <- PromptBar <$> getWidget builder boxName
                           <*> getWidget builder labelName
                           <*> pure entry
                           <*> newSignal Changed
                           <*> pure closedSignal
                           <*> pure validated

    onEntryChanged entry $ emit (promptBar^.changed_)
    onEntryCanceled entry . async $ close promptBar
    onEntryValidated entry $ emit validated

    return promptBar


-- | Widget name used in the XML file that describes the UI
labelName, entryName, boxName :: Text
labelName = "promptDescription"
entryName = "promptEntry"
boxName   = "promptBox"

initialize :: (MonadIO m) => PromptBar -> m PromptBar
initialize =
    withM_ description_ (gAsync . (`labelSetAttributes` [allItalic, allBold]))
    >=> withM_ description_ (gAsync . (`labelSetAttributes` [AttrForeground {paStart = 0, paEnd = -1, paColor = gray}]))
    >=> withM_ entry_ (gAsync . (\e -> widgetModifyBase e StateNormal black))
    >=> withM_ entry_ (gAsync . (\e -> widgetModifyText e StateNormal gray))


open :: (MonadIO m) => Text -> Text -> PromptBar -> m PromptBar
open description defaultText =
    withM_ description_ (gAsync . (`labelSetText` description))
    >=> withM_ entry_ (gAsync . (`entrySetText` defaultText))
    >=> withM_ box_ (gAsync . widgetShow)
    >=> withM_ entry_ (gAsync . widgetGrabFocus)
    >=> withM_ entry_ (gAsync . (`editableSetPosition` (-1)))

close :: (ControlIO m, MonadLogger m) => PromptBar -> m PromptBar
close promptBar = do
  runMaybeT $ do
    guard =<< get (promptBar^.box_) widgetVisible
    emit (promptBar^.closed_) ()
    gAsync . widgetHide $ promptBar^.box_
    void $ clean promptBar
  return promptBar

-- | Close prompt, that is: clean its content, signals and callbacks
clean :: (ControlIO m) => PromptBar -> m PromptBar
clean = withM_ entry_ (gAsync . (`widgetRestoreText` StateNormal))
    >=> withM_ entry_ (gAsync . (\e -> widgetModifyText e StateNormal gray))


-- {{{ Prompts
-- | Open prompt bar with given description and default value,
-- register a callback to trigger when value is changed, and another one when value is validated.
prompt :: (ControlIO m, MonadLogger m, MonadThrow m)
        => Text             -- ^ Prompt description
        -> Text             -- ^ Pre-fill value
        -> PromptBar
        -> m Text
prompt description startValue promptBar = do
    clean promptBar
    open description startValue promptBar

    cancelation <- listenTo $ promptBar^.closed_
    validation  <- listenTo $ promptBar^.validated_

    result <- io $ waitEitherCancel cancelation validation
    close promptBar
    maybe (throwM PromptInterrupted) return . join $ hush result

promptM :: (ControlIO m, MonadReader r m, Has PromptBar r, MonadLogger m, MonadThrow m)
        => Text -> Text -> m Text
promptM a b = prompt a b =<< ask


iprompt :: (ControlIO m, MonadLogger m, MonadResource m)
        => Text
        -> Text
        -> (Text -> m ())
        -> PromptBar
        -> m ()
iprompt description startValue f promptBar = do
    clean promptBar

    update <- addHandler (promptBar^.changed_) f
    open description startValue promptBar

    io . wait =<< listenTo (promptBar^.closed_)
    close promptBar
    release update

ipromptM :: (ControlIO m, MonadResource m, MonadReader r m, Has PromptBar r, MonadLogger m)
         => Text -> Text -> (Text -> m ()) -> m ()
ipromptM a b c = iprompt a b c =<< ask


-- | Same as 'prompt' for URI values
uriPrompt :: (ControlIO m, MonadLogger m, MonadResource m)
          => Text
          -> Text
          -> PromptBar
          -> m URI
uriPrompt description startValue promptBar = do
    clean promptBar

    update <- addHandler (promptBar^.changed_) $ checkURI promptBar
    open description startValue promptBar

    validation  <- listenTo $ promptBar^.validated_
    cancelation <- listenTo $ promptBar^.closed_

    result <- io $ waitEitherCancel cancelation validation
    release update
    close promptBar
    parseURIReference =<< maybe (throwM PromptInterrupted) return (join $ hush result)


uriPromptM :: (ControlIO m, MonadReader r m, Has PromptBar r, MonadLogger m, MonadResource m)
           => Text -> Text -> m URI
uriPromptM a b = uriPrompt a b =<< ask


checkURI :: (MonadIO m, MonadLogger m) => PromptBar -> Text -> m ()
checkURI promptBar v = do
    debug $ "Is URI ? " <> show (isURIReference $ unpack v)
    gAsync $ widgetModifyText (promptBar^.entry_) StateNormal (if isURIReference (unpack v) then green else red)


getPromptValue :: (MonadIO m) => PromptBar -> m Text
getPromptValue = gSync . entryGetText . view entry_

getPromptValueM :: (MonadIO m, MonadReader r m, Has PromptBar r) => m Text
getPromptValueM = getPromptValue =<< ask


onEntryCanceled :: (ControlIO m, MonadLogger m, EntryClass t) => t -> m a -> m ()
onEntryCanceled entry f = liftBaseWith $ \runInIO -> gAsync . on entry keyPressEvent $ do
    key <- KeyVal <$> eventKeyVal
    io . when (key == _Escape) . void . runInIO $ void f
    return False

onEntryChanged :: (ControlIO m, MonadLogger m, EditableClass t, EntryClass t) => t -> (Text -> m ()) -> m ()
onEntryChanged entry f = liftBaseWith $ \runInIO -> gAsync . on entry editableChanged $ do
    value <- entryGetText entry
    void . runInIO $ f value

onEntryValidated :: (ControlIO m, MonadLogger m, EntryClass t) => t -> (Text -> m ()) -> m ()
onEntryValidated entry f = liftBaseWith $ \runInIO -> gAsync . on entry entryActivated $ do
    value <- entryGetText entry
    void . runInIO $ f value
