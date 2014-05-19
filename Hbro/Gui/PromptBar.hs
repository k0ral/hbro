{-# LANGUAGE FlexibleInstances, TemplateHaskell, DeriveDataTypeable #-}
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
import Hbro.Error
import Hbro.Gui.Buildable
import Hbro.Gui.PromptBar.Signals
import Hbro.Gui.PromptBar.Hooks hiding(clean, set)
import qualified Hbro.Gui.PromptBar.Hooks as Hooks
import Hbro.Util

import Control.Concurrent.MVar
import Control.Lens.Getter
import Control.Lens.Setter
import Control.Lens.Lens
import Control.Lens.TH
import Control.Monad.Reader hiding(when)

import Graphics.Rendering.Pango.Enums
import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk.Display.Label
import Graphics.UI.Gtk.Entry.Editable
import Graphics.UI.Gtk.Entry.Entry
import Graphics.UI.Gtk.Layout.HBox

import Network.URI.Monadic

import Prelude hiding(mapM_, read)
-- }}}

-- {{{ Types
-- | No exported constructor, please use 'buildWith'
data PromptBar = PromptBar
    { _box         :: HBox
    , _description :: Label
    , _entry       :: Entry
    }

makeLensesWith ?? ''PromptBar $ classyRules
    & lensField .~ (\name -> Just (tail name ++ "L"))
    & lensClass .~ (\name -> Just ("Has" ++ name, "_" ++ map toLower name))

-- | A 'PromptBar' can be built from an XML file.
instance Buildable PromptBar where
    buildWith b = PromptBar <$> gSync (builderGetObject b castToHBox boxName)
                            <*> gSync (builderGetObject b castToLabel labelName)
                            <*> gSync (builderGetObject b castToEntry entryName)

-- | Widget name used in the XML file that describes the UI
labelName, entryName, boxName :: String
labelName = "promptDescription"
entryName = "promptEntry"
boxName   = "promptBox"

data PromptInterrupted = PromptInterrupted deriving(Typeable)
instance Exception PromptInterrupted
instance Show PromptInterrupted where show _ = "Prompt interrupted."
-- }}}

initialize :: (MonadBase IO m) => PromptBar -> m ()
initialize aPromptBar = gAsync $ do
    description `labelSetAttributes` [allItalic, allBold]
    description `labelSetAttributes` [AttrForeground {paStart = 0, paEnd = -1, paColor = gray}]
    widgetModifyBase entry StateNormal black
    widgetModifyText entry StateNormal gray
  where
    description = aPromptBar^.descriptionL
    entry       = aPromptBar^.entryL


-- | Pops-up the prompt bar, filling it with given arguments.
open :: (MonadBase IO m, MonadReader t m, HasPromptBar t)
     => String   -- ^ Description
     -> String   -- ^ Pre-fill value
     -> m ()
open a b = do
    io $ debugM "hbro.promptbar" "Opening prompt."
    void . open' a b =<< askl _promptbar

open' :: (MonadBase IO m) => String -> String -> PromptBar -> m PromptBar
open' newDescription defaultText =
    withM_ descriptionL (gAsync . (`labelSetText` newDescription))
        >=> withM_ entryL (gAsync . (`entrySetText` defaultText))
        >=> withM_ boxL (gAsync . widgetShow)
        >=> withM_ entryL (gAsync . widgetGrabFocus)
        >=> withM_ entryL (gAsync . (`editableSetPosition` (-1)))

unhide, hide :: (MonadBase IO m, MonadReader t m, HasPromptBar t) => m ()
unhide = gAsync . widgetShow =<< askl (_promptbar.boxL)
hide   = gAsync . widgetHide =<< askl (_promptbar.boxL)

-- | Close prompt, that is: clean its content, signals and callbacks
clean :: (MonadBase IO m, MonadReader t m, HasPromptBar t, HasPromptHooks n t) => m ()
clean = do
    gAsync . (`widgetRestoreText` StateNormal)           =<< askl (_promptbar.entryL)
    gAsync . (\e -> widgetModifyText e StateNormal gray) =<< askl (_promptbar.entryL)

    hide
    Hooks.clean


-- {{{ Prompts
-- | Same as 'prompt\'' without the value-changed callback
prompt :: (MonadBase IO m, MonadThrow m, MonadReader t m, HasPromptBar t, HasPromptHooks n t, MonadBase IO n)
     => String                       -- ^ Prompt description
     -> String                       -- ^ Initial value
     -> m String
prompt a b = prompt' a b (const $ return ())


-- | Open prompt bar with given description and default value,
-- register a callback to trigger when value is changed, and another one when value is validated.
prompt' :: (MonadBase IO m, MonadThrow m, MonadReader t m, HasPromptBar t, HasPromptHooks n t, MonadBase IO n)
        => String             -- ^ Prompt description
        -> String             -- ^ Pre-fill value
        -> (String -> n ())   -- ^ Callback triggered each time prompt value changes
        -> m String
prompt' description startValue f = do
    clean
    open description startValue
    Hooks.set onChangedL $ \(Changed x) -> f x

    result <- io newEmptyMVar
    Hooks.set onCancelledL $ \_ -> io (putMVar result Nothing)
    Hooks.set onValidatedL $ \(Activated x) -> io (putMVar result (Just x))
    io (takeMVar result) `failWithM` PromptInterrupted


-- | Same as 'prompt\'' for URI values
promptURI :: (MonadBase IO m, MonadBase IO n, MonadThrow m, MonadThrow n, MonadReader t m, MonadReader t n, HasPromptBar t, HasPromptHooks n t)
          => String -> String -> m URI
promptURI description startValue = do
    clean
    open description startValue
    checkURI (Changed startValue)

    Hooks.set onChangedL checkURI

    result <- io newEmptyMVar
    Hooks.set onCancelledL $ \_ -> io (putMVar result Nothing)
    Hooks.set onValidatedL $ \(Activated uri) -> io (putMVar result (Just uri))
    parseURIReference =<< io (takeMVar result) `failWithM` PromptInterrupted


checkURI :: (MonadBase IO m, MonadReader t m, HasPromptBar t) => Changed -> m ()
checkURI (Changed v) = do
    io . debugM "hbro.prompt" $ "Is URI ? " ++ show (isURIReference v)
    (gAsync . \e -> widgetModifyText e StateNormal (green <| isURIReference v |> red)) =<< askl (_promptbar.entryL)


getEntryValue :: (MonadBase IO m, MonadReader t m, HasPromptBar t) => m String
getEntryValue = gSync . entryGetText =<< askl (_promptbar.entryL)
