{-# LANGUAGE DeriveDataTypeable, RankNTypes, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
module Hbro.Types where

-- {{{ Imports
-- import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Trans.Control

import Data.Dynamic
import Data.IORef
import Data.Map
import Data.Maybe
import Data.Monoid
--import Data.Set

import Graphics.UI.Gtk.Abstract.Object
import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk.Display.Label
import Graphics.UI.Gtk.Entry.Entry
import Graphics.UI.Gtk.General.General
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.Layout.HBox
import Graphics.UI.Gtk.Scrolling.ScrolledWindow
import Graphics.UI.Gtk.WebKit.Download
import Graphics.UI.Gtk.WebKit.NetworkRequest
import Graphics.UI.Gtk.WebKit.WebFrame
import Graphics.UI.Gtk.WebKit.WebNavigationAction
import Graphics.UI.Gtk.WebKit.WebPolicyDecision
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.Windows.Window

import Network.URI

import System.Console.CmdArgs
import System.Glib.GObject
import System.Glib.Signals
import System.IO.Error
import qualified System.ZMQ as ZMQ
-- }}}

-- {{{ Error
data HError =
    CannotGoBack
  | CannotGoForward
  | EmptyCallback
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
    show CannotGoBack         = "Unable to go back: already at oldest page."
    show CannotGoForward      = "Unable to go forward: already at newest page."
    show (IOE e)              = "IO error: " ++ ioeGetLocation e ++ ": " ++ fromMaybe "" (ioeGetFileName e) ++ " " ++ ioeGetErrorString e
    show InvalidIconURI       = "No favicon URI."
    show InvalidPageTitle     = "No page title."
    show InvalidPageURI       = "Invalid page URI."
    show (InvalidURI s)       = show s
    show (EmptyDownloadURI _) = "Invalid download URI."
    show (EmptySuggestedFileName _) = "No suggested name for this download."
    show (EmptyRequestURI _)  = "Invalid request URI."
    show EmptyCallback        = "No callback defined."
    show (OtherError s)       = show s
-- }}}

data Context = Context {
    __options    :: CliOptions,                 -- ^ Commandline options
    __config     :: Config,                     -- ^ Custom configuration provided by user
    __UI         :: GUI,
    __ZMQContext :: ZMQ.Context,
    __hooks      :: Hooks,
    __keys       :: IORef String
}

-- {{{ Commandline options
-- | Available commandline options (cf hbro -h).
data CliOptions = CliOptions {
    __startURI     :: Maybe String,
    __vanilla      :: Bool,
    __recompile    :: Bool,
    __denyReconf   :: Bool,
    __forceReconf  :: Bool,
    __dyreDebug    :: Bool,
    __masterBinary :: Maybe String
} deriving (Data, Typeable, Show, Eq)


class HasOptions m where
    _startURI   :: m -> Maybe String
    _vanilla    :: m -> Bool
    _recompile  :: m -> Bool

instance HasOptions CliOptions where
    _startURI  = __startURI
    _vanilla   = __vanilla
    _recompile = __recompile

instance HasOptions Context where
    _startURI  = __startURI . __options
    _vanilla   = __vanilla . __options
    _recompile = __recompile . __options
-- }}}

-- {{{ Configuration types
-- | Custom settings provided by the user.
data Config = Config {
    __socketDir :: IO FilePath,             -- ^ Directory where ZeroMQ sockets will be created ("/tmp" for example)
    __UIFile    :: IO FilePath,             -- ^ Path to XML file describing UI (used by GtkBuilder)
    __homePage  :: URI,                     -- ^ Startup page
    __commands  :: CommandsList             -- ^ Commands recognized through 0MQ sockets
}


class HasConfig m where
    _socketDir :: m -> IO FilePath
    _UIFile    :: m -> IO FilePath
    _homePage  :: m -> URI
    _commands  :: m -> CommandsList

instance HasConfig Config where
    _socketDir = __socketDir
    _UIFile    = __UIFile
    _homePage  = __homePage
    _commands  = __commands

instance HasConfig Context where
    _socketDir = __socketDir . __config
    _UIFile    = __UIFile . __config
    _homePage  = __homePage . __config
    _commands  = __commands . __config


class HasKeys m where
    _keys :: m -> IORef String

instance HasKeys Context where
    _keys = __keys

class HasZMQContext m where
    _ZMQContext :: m -> ZMQ.Context

instance HasZMQContext Context where
    _ZMQContext = __ZMQContext
-- }}}

-- {{{ UI types
-- | UI elements that can be built from GtkBuilder.
class Buildable a where
    build :: Builder -> ((MonadIO m) => m a)


data GUI = GUI {
    __mainWindow         :: Window,
    __inspectorWindow    :: Window,
    __scrollWindow       :: ScrolledWindow,  -- ^ 'ScrolledWindow' containing the webview
    __webView            :: WebView,
    __promptBar          :: PromptBar,
    __statusBar          :: StatusBar,       -- ^ Status bar's horizontal box
    __notificationBar    :: NotificationBar,
    __builder            :: Builder          -- ^ Builder object created from XML file
}

newtype StatusBar = StatusBar HBox

instance GObjectClass StatusBar where
    toGObject (StatusBar h) = toGObject h
    unsafeCastGObject g     = StatusBar $ unsafeCastGObject g

instance ObjectClass StatusBar
instance WidgetClass StatusBar

class HasGUI m where
    _mainWindow      :: m -> Window
    _inspectorWindow :: m -> Window
    _scrollWindow    :: m -> ScrolledWindow
    _webView         :: m -> WebView
    _promptBar       :: m -> PromptBar
    _statusBar       :: m -> StatusBar
    _notificationBar :: m -> NotificationBar
    _builder         :: m -> Builder


instance HasGUI GUI where
    _mainWindow      = __mainWindow
    _inspectorWindow = __inspectorWindow
    _scrollWindow    = __scrollWindow
    _webView         = __webView
    _promptBar       = __promptBar
    _statusBar       = __statusBar
    _notificationBar = __notificationBar
    _builder         = __builder


instance HasGUI Context where
    _mainWindow      = __mainWindow      . __UI
    _inspectorWindow = __inspectorWindow . __UI
    _scrollWindow    = __scrollWindow    . __UI
    _webView         = __webView         . __UI
    _promptBar       = __promptBar       . __UI
    _statusBar       = __statusBar       . __UI
    _notificationBar = __notificationBar . __UI
    _builder         = __builder         . __UI


class HasWebView m where
    _webview  :: m -> WebView

instance (HasGUI m) => HasWebView m where
    _webview = _webView


class HasScrollWindow m where
    _scrollwindow :: m -> ScrolledWindow

instance (HasGUI m) => HasScrollWindow m where
    _scrollwindow = _scrollWindow


data PromptBar = PromptBar {
    _box                    :: HBox,
    _description            :: Label,
    _entry                  :: Entry
}

class HasPromptBar m where
    _promptBox         :: m -> HBox
    _promptDescription :: m -> Label
    _promptEntry       :: m -> Entry

instance HasPromptBar PromptBar where
    _promptBox         = _box
    _promptDescription = _description
    _promptEntry       = _entry

instance HasPromptBar GUI where
    _promptBox         = _box . _promptBar
    _promptDescription = _description . _promptBar
    _promptEntry       = _entry . _promptBar

instance HasPromptBar Context where
    _promptBox         = _box . _promptBar . __UI
    _promptDescription = _description . _promptBar . __UI
    _promptEntry       = _entry . _promptBar . __UI


data NotificationBar = NotificationBar {
    _label   :: Label                          -- ^ Content
}

class HasNotificationBar m where
    _notificationbar :: m -> NotificationBar

instance (HasGUI m) => HasNotificationBar m where
    _notificationbar = _notificationBar
-- }}}

-- {{{ Hooks
data Hooks = Hooks {
    __notificationTimer :: IORef (Maybe HandlerId),
    __promptChanged     :: IORef (Maybe (ConnectId Entry)),
    __promptValidated   :: IORef (Maybe (ConnectId Entry))
}


class HasHooks m where
--    _custom               :: IORef (Map String Dynamic)
    _notificationTimer  :: m -> IORef (Maybe HandlerId)
    _promptChanged      :: m -> IORef (Maybe (ConnectId Entry))
    _promptValidated    :: m -> IORef (Maybe (ConnectId Entry))

instance HasHooks (a, Hooks) where
    _notificationTimer = __notificationTimer . snd
    _promptChanged     = __promptChanged . snd
    _promptValidated   = __promptValidated . snd


instance HasHooks Context where
    _notificationTimer = __notificationTimer . __hooks
    _promptChanged     = __promptChanged . __hooks
    _promptValidated   = __promptValidated . __hooks


newtype Setup           = Setup (forall r m. (MonadIO m, MonadReader r m, HasConfig r, HasOptions r, HasGUI r, HasPromptBar r, HasZMQContext r, HasHooks r, HasKeys r, MonadError HError m, MonadBaseControl IO m) => m ())
type ClipboardHook      = String -> (forall m. (MonadIO m, MonadError HError m, MonadBaseControl IO m) => m ())
type EntryHook          = String -> (forall r m. (MonadIO m, MonadBaseControl IO m, MonadError HError m, MonadReader r m, HasConfig r, HasGUI r, HasPromptBar r, HasOptions r, HasZMQContext r, HasHooks r) => m ())
type EntryURIHook       = URI -> (forall r m. (MonadIO m, MonadError HError m, MonadBaseControl IO m, MonadReader r m, HasConfig r, HasGUI r, HasPromptBar r, HasOptions r, HasZMQContext r, HasHooks r) => m ())
newtype DownloadHook       = DownloadHook (URI -> String -> Int -> forall r m. (MonadIO m, Functor m, MonadReader r m, HasConfig r, HasOptions r, HasGUI r, HasZMQContext r, HasHooks r, MonadError HError m, MonadBaseControl IO m) => m ())
type KeyHook            = String -> forall r m. (MonadIO m, MonadBaseControl IO m, MonadError HError m, MonadReader r m, HasConfig r, HasGUI r, HasPromptBar r, HasOptions r, HasZMQContext r, HasHooks r, HasKeys r) => m ()
newtype LoadFinishedHook   = LoadFinishedHook (forall r m. (MonadIO m, MonadReader r m, HasConfig r, HasOptions r, HasGUI r, HasZMQContext r, HasHooks r, MonadError HError m, MonadBaseControl IO m) => m ())
newtype NavigationHook     = NavigationHook (NavigationReason -> Maybe MouseButton -> URI -> WebPolicyDecision -> forall r m. (MonadIO m, Functor m, MonadReader r m, HasConfig r, HasOptions r, HasGUI r, HasZMQContext r, MonadError HError m, MonadBaseControl IO m) => m ())
newtype NewWebViewHook     = NewWebViewHook (WebFrame -> forall r m. (MonadIO m, Functor m, MonadReader r m, HasConfig r, HasOptions r, HasGUI r, HasZMQContext r, MonadError HError m, MonadBaseControl IO m) => m WebView)
newtype NewWindowHook      = NewWindowHook (WebFrame -> NetworkRequest -> WebNavigationAction -> WebPolicyDecision -> forall r m. (MonadIO m, Functor m, MonadReader r m, HasConfig r, HasOptions r, HasGUI r, HasZMQContext r, MonadError HError m, MonadBaseControl IO m) => m ())
newtype ResourceOpenedHook = ResourceOpenedHook (URI -> String -> WebPolicyDecision -> forall r m. (MonadIO m, MonadReader r m, HasConfig r, HasOptions r, HasGUI r, HasZMQContext r, MonadError HError m, MonadBaseControl IO m) => m ())
newtype TitleChangedHook   = TitleChangedHook (String -> forall r m. (MonadIO m, MonadReader r m, HasConfig r, HasOptions r, HasGUI r, HasZMQContext r, MonadError HError m, MonadBaseControl IO m) => m ())
-- }}}

-- {{{ Missing instances from webkit
instance Eq NavigationReason where
  a == b = (fromEnum a) == (fromEnum b)

instance Show NavigationReason where
  show WebNavigationReasonLinkClicked   = "Link clicked"
  show WebNavigationReasonFormSubmitted = "Form submitted"
  show WebNavigationReasonBackForward   = "Back/forward"
  show WebNavigationReasonReload        = "Reload"
  show WebNavigationReasonFormResubmitted = "Form resubmitted"
  show WebNavigationReasonOther         = "Other"
-- }}}

-- {{{ Keys
-- Note: for modifiers, lists are used for convenience purposes,
-- but are transformed into sets internally, so that order and repetition don't matter.
-- | List of bound keys.
-- All callbacks are fed with the Context instance.
newtype KeysList      = KeysList (forall r m. (MonadIO m, MonadReader r m, HasConfig r, HasOptions r, HasGUI r, HasPromptBar r, HasZMQContext r, HasHooks r, MonadError HError m, MonadBaseControl IO m) => [(String, m ())])

instance Monoid KeysList where
    mempty = KeysList []
    mappend (KeysList a) (KeysList b) = KeysList (mappend a b)

--type KeysMap          = (MonadIO m, MonadReader Context m) => Map String (m ())
--data KeyMode          = CommandMode | InsertMode
-- }}}

newtype CommandsList = CommandsList ((Functor m, MonadIO m, MonadReader r m, HasConfig r, HasOptions r, HasGUI r, HasZMQContext r, MonadError HError m) => [(String, ([String] -> m String))])
type CommandsMap  = (MonadIO m, MonadReader Context m) => Map String ([String] -> m String)

-- Boolean datatypes
data CaseSensitivity = CaseSensitive | CaseInsensitive
data Direction       = Forward       | Backward
data Wrap            = Wrap          | NoWrap

data Axis     = Horizontal | Vertical
data Position = Absolute Double | Relative Double
