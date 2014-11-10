{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell    #-}
-- | Designed to be imported as @qualified@.
module Hbro.Gui.PromptBar (
-- * Types
      PromptBar
    -- description
    , HasPromptBar(..)
    , labelName
    , entryName
    , boxName
-- * Functions
    , initialize
    , open
    , unhide
    , hide
    , Hbro.Gui.PromptBar.clean
-- * Prompts
    , prompt
    , prompt'
    , promptURI
    , getEntryValue
) where

-- {{{ Imports
import           Hbro.Error
import           Hbro.Event
import           Hbro.Gui.Buildable
import           Hbro.Gui.PromptBar.Hooks        hiding (clean, set)
import qualified Hbro.Gui.PromptBar.Hooks        as Hooks
import           Hbro.Gui.PromptBar.Signals
import           Hbro.Logger                     hiding (initialize)
import           Hbro.Prelude

import           Control.Lens.Getter
import           Control.Lens.TH

import           Graphics.Rendering.Pango.Enums
import           Graphics.UI.Gtk.Abstract.Widget
import           Graphics.UI.Gtk.Builder
import           Graphics.UI.Gtk.Display.Label
import           Graphics.UI.Gtk.Entry.Editable
import           Graphics.UI.Gtk.Entry.Entry
import           Graphics.UI.Gtk.Layout.HBox

import           Network.URI.Monadic
-- }}}

-- {{{ Types
-- | No exported constructor, please use 'buildWith'
declareClassy [d|
  data PromptBar = PromptBar
    { boxL         :: HBox
    , descriptionL :: Label
    , entryL       :: Entry
    }
  |]

-- | A 'PromptBar' can be built from an XML file.
instance Buildable PromptBar where
    buildWith b = PromptBar <$> gSync (builderGetObject b castToHBox boxName)
                            <*> gSync (builderGetObject b castToLabel labelName)
                            <*> gSync (builderGetObject b castToEntry entryName)

-- | Widget name used in the XML file that describes the UI
labelName, entryName, boxName :: Text
labelName = "promptDescription"
entryName = "promptEntry"
boxName   = "promptBox"

-- | Error message
promptInterrupted :: Text
promptInterrupted = "Prompt interrupted."
-- }}}

initialize :: (BaseIO m) => PromptBar -> m ()
initialize aPromptBar = gAsync $ do
    description `labelSetAttributes` [allItalic, allBold]
    description `labelSetAttributes` [AttrForeground {paStart = 0, paEnd = -1, paColor = gray}]
    widgetModifyBase entry StateNormal black
    widgetModifyText entry StateNormal gray
  where
    description = aPromptBar^.descriptionL
    entry       = aPromptBar^.entryL


-- | Pops-up the prompt bar, filling it with given arguments.
open :: (BaseIO m, MonadReader t m, HasPromptBar t)
     => Text   -- ^ Description
     -> Text   -- ^ Pre-fill value
     -> m ()
open a b = do
    debugM "hbro.promptbar" "Opening prompt."
    void . open' a b =<< askL promptBar

open' :: (BaseIO m) => Text -> Text -> PromptBar -> m PromptBar
open' newDescription defaultText =
    withM_ descriptionL (gAsync . (`labelSetText` newDescription))
        >=> withM_ entryL (gAsync . (`entrySetText` defaultText))
        >=> withM_ boxL (gAsync . widgetShow)
        >=> withM_ entryL (gAsync . widgetGrabFocus)
        >=> withM_ entryL (gAsync . (`editableSetPosition` (-1)))

unhide, hide :: (BaseIO m, MonadReader t m, HasPromptBar t) => m ()
unhide = gAsync . widgetShow =<< askL (promptBar.boxL)
hide   = gAsync . widgetHide =<< askL (promptBar.boxL)

-- | Close prompt, that is: clean its content, signals and callbacks
clean :: (BaseIO m, MonadReader t m, HasPromptBar t, HasPromptHooks n t) => m ()
clean = do
    gAsync . (`widgetRestoreText` StateNormal)           =<< askL (promptBar.entryL)
    gAsync . (\e -> widgetModifyText e StateNormal gray) =<< askL (promptBar.entryL)

    hide
    Hooks.clean


-- {{{ Prompts
-- | Same as 'prompt\'' without the value-changed callback
prompt :: (BaseIO m, MonadError Text m, MonadReader t m, HasPromptBar t, HasPromptHooks n t, BaseIO n)
     => Text                       -- ^ Prompt description
     -> Text                       -- ^ Initial value
     -> m Text
prompt a b = prompt' a b nullHook


-- | Open prompt bar with given description and default value,
-- register a callback to trigger when value is changed, and another one when value is validated.
prompt' :: (BaseIO m, MonadError Text m, MonadReader t m, HasPromptBar t, HasPromptHooks n t, BaseIO n)
        => Text             -- ^ Prompt description
        -> Text             -- ^ Pre-fill value
        -> Hook n Changed   -- ^ Callback triggered each time prompt value changes
        -> m Text
prompt' description startValue f = do
    clean
    open description startValue
    result <- newEmptyMVar

    Hooks.set onChangedL f
    Hooks.set onCancelledL . Hook $ \_ -> putMVar result Nothing
    Hooks.set onValidatedL . Hook $ putMVar result . Just
    takeMVar result <!> promptInterrupted


-- | Same as 'prompt\'' for URI values
promptURI :: (BaseIO m, BaseIO n, MonadError Text m, MonadError Text n, MonadReader t m, MonadReader t n, HasPromptBar t, HasPromptHooks n t)
          => Text -> Text -> m URI
promptURI description startValue = do
    clean
    open description startValue
    checkURI startValue

    result <- newEmptyMVar

    Hooks.set onChangedL $ Hook checkURI
    Hooks.set onCancelledL . Hook $ \_ -> putMVar result Nothing
    Hooks.set onValidatedL . Hook $ putMVar result . Just

    parseURIReference =<< takeMVar result <!> promptInterrupted


checkURI :: (BaseIO m, MonadReader t m, HasPromptBar t) => Text -> m ()
checkURI v = do
    debugM "hbro.prompt" $ "Is URI ? " ++ tshow (isURIReference $ unpack v)
    (gAsync . \e -> widgetModifyText e StateNormal (green <| isURIReference (unpack v) |> red)) =<< askL (promptBar.entryL)


getEntryValue :: (BaseIO m, MonadReader t m, HasPromptBar t) => m Text
getEntryValue = gSync . entryGetText =<< askL (promptBar.entryL)
