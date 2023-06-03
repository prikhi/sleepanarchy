module App
  ( AppM
  , runAppM
  , AppEnv(..)
  , class HasNav
  , getNav
  , class Navigation
  , newUrl
  , openInNewTab
  , class HasMarkdown
  , getMarkdown
  , class Markdown
  , renderMarkdown
  , renderMarkdownUnsafe
  , mkMarkdownInstance
  , class GetTime
  , getToday
  , class FileUpload
  , encodeBase64
  , clearInputValue
  , class HasAuthStatus
  , getAuthStatusRef
  , class Auth
  , isLoggedIn
  , setLoggedIn
  , setLoggedOut
  , AuthStatus(..)
  , initializeAuthStatus
  , class HasPageDataSubscription
  , pageDataEmitter
  , pageDataNotifier
  , class PageDataListener
  , pageDataActionEmitter
  , notifyPageDataAfterMs
  , killDelayedPageDataNotif
  , PageDataDelayedNotif
  , class PageDataNotifier
  , pageDataReceived
  , mkPageDataNotifierEval
  ) where

import Prelude

import Control.Monad.Reader
  ( class MonadAsk
  , class MonadReader
  , ReaderT
  , asks
  , runReaderT
  )
import Data.Argonaut
  ( class DecodeJson
  , class EncodeJson
  , decodeJson
  , encodeJson
  , parseJson
  , stringify
  )
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Date (Date)
import Data.Either (hush)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe, fromMaybe)
import Data.Options ((:=))
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.Time.Duration (Milliseconds)
import Data.Traversable (for)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, delay, error, forkAff, killFiber)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Now (nowDate)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Foreign (unsafeToForeign)
import Halogen as H
import Halogen.Component (EvalSpec)
import Halogen.Subscription (Emitter, Listener, SubscribeIO, notify)
import Highlight as Highlight
import MarkdownIt (MarkdownIt)
import MarkdownIt as Markdown
import Network.RemoteData
  ( RemoteData
  , isFailure
  , isLoading
  , isNotAsked
  , isSuccess
  )
import Router (Route, reverse)
import Routing.PushState (PushStateInterface)
import Web.DOM (Element)
import Web.Event.Event (preventDefault)
import Web.File.File (File, toBlob)
import Web.File.FileReader.Aff (readAsDataURL)
import Web.HTML (window)
import Web.HTML.Window (localStorage, open)
import Web.Storage.Storage as Storage
import Web.UIEvent.MouseEvent as ME

-- APP MONAD

-- | The context the Halogen app operates within.
newtype AppM a = AppM (ReaderT AppEnv Aff a)

-- | Run the given app context with the given environment.
runAppM :: forall a. AppM a -> AppEnv -> Aff a
runAppM (AppM m) = runReaderT m

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM
derive newtype instance monadAskAppM :: MonadAsk AppEnv AppM
derive newtype instance monadReaderAppM :: MonadReader AppEnv AppM

-- APP ENVIRONMENT

-- | Runtime values used by the app.
data AppEnv =
  Env
    { nav :: PushStateInterface
    , md :: MarkdownIt
    , authStatus :: Ref AuthStatus
    , pageDataSub :: SubscribeIO Unit
    }

-- NAVIGATION

class HasNav a where
  getNav :: a -> PushStateInterface

instance hasNavAppEnv :: HasNav AppEnv where
  getNav (Env e) = e.nav

class Monad m <= Navigation m where
  -- | Set the page's URL to the URL for the given Route & call
  -- | `preventDefault` on the given event if present.
  newUrl :: Route -> Maybe ME.MouseEvent -> m Unit
  -- | Open the given URL in a new browser tab.
  openInNewTab :: String -> m Unit

instance navigationHasNav ::
  ( HasNav env
  , MonadAsk env m
  , MonadEffect m
  ) =>
  Navigation m where
  newUrl url mbEvent = do
    void $ for mbEvent $ ME.toEvent >>> preventDefault >>> liftEffect
    nav <- asks getNav
    liftEffect $ nav.pushState (unsafeToForeign {}) $ reverse url
  openInNewTab url =
    liftEffect $ window >>= open url "_blank" "" >>> void

-- MARKDOWN

class HasMarkdown a where
  getMarkdown :: a -> MarkdownIt

instance hasMarkdownAppEnv :: HasMarkdown AppEnv where
  getMarkdown (Env e) = e.md

class Monad m <= Markdown m where
  -- | Render the given Markdown String into an HTML String
  renderMarkdown :: String -> m String

instance markdownHasMarkdown ::
  ( HasMarkdown env
  , MonadAsk env m
  , MonadEffect m
  ) =>
  Markdown m where
  renderMarkdown mdString = do
    md <- asks getMarkdown
    liftEffect $ Markdown.render md mdString

-- | Build a markdown renderer for use throughout the app.
mkMarkdownInstance :: Effect MarkdownIt
mkMarkdownInstance = Markdown.newMarkdownIt Markdown.CommonMark $
  (Markdown.html := true)
    <> (Markdown.typographer := true)
    <> (Markdown.highlight := Highlight.highlight)

-- | Unsafely render some markdown into an HTML string.
renderMarkdownUnsafe :: String -> String
renderMarkdownUnsafe str = unsafePerformEffect $ do
  renderer <- mkMarkdownInstance
  Markdown.render renderer str

-- DATES

class Monad m <= GetTime m where
  getToday :: m Date

instance monadEffectGetTime :: MonadEffect m => GetTime m where
  getToday = liftEffect nowDate

-- FILE UPLOADS

class Monad m <= FileUpload m where
  encodeBase64 :: File -> m String
  clearInputValue :: Element -> m Unit

instance monadEffectFileUpload :: MonadAff m => FileUpload m where
  encodeBase64 file = liftAff $ do
    dataUrl <- readAsDataURL $ toBlob file
    pure $ String.drop 1 $ String.dropWhile (_ /= String.codePointFromChar ',')
      dataUrl
  clearInputValue = liftEffect <<< _clearInputValue

foreign import _clearInputValue :: Element -> Effect Unit

-- AUTH

data AuthStatus
  = Anonymous
  | Authorized

derive instance genericAuthStatus :: Generic AuthStatus _
derive instance eqAuthStatus :: Eq AuthStatus
instance showAuthStatus :: Show AuthStatus where
  show = genericShow

instance encodeJsonAuthStatus :: EncodeJson AuthStatus where
  encodeJson = genericEncodeJson

instance decodeJsonAuthStatus :: DecodeJson AuthStatus where
  decodeJson = genericDecodeJson

authStatusLocalStorageKey :: String
authStatusLocalStorageKey = "AUTHSTATUS"

class HasAuthStatus a where
  getAuthStatusRef :: a -> Ref AuthStatus

instance hasAuthStatusAppEnv :: HasAuthStatus AppEnv where
  getAuthStatusRef (Env e) = e.authStatus

initializeAuthStatus :: forall m. MonadEffect m => m (Ref AuthStatus)
initializeAuthStatus = liftEffect $ do
  storage <- window >>= localStorage
  mbRawVal <- Storage.getItem authStatusLocalStorageKey storage
  let mbStatus = mbRawVal >>= (parseJson >>> hush) >>= (decodeJson >>> hush)
  Ref.new $ fromMaybe Anonymous mbStatus

class Monad m <= Auth m where
  setLoggedIn :: m Unit
  setLoggedOut :: m Unit
  isLoggedIn :: m Boolean

instance authHasAuthStatusEff ::
  ( HasAuthStatus env
  , MonadAsk env m
  , MonadEffect m
  ) =>
  Auth m where
  isLoggedIn = do
    ref <- asks getAuthStatusRef
    liftEffect $ ((==) Authorized) <$> Ref.read ref
  setLoggedIn = do
    asks getAuthStatusRef >>= Ref.write Authorized >>> liftEffect
    storage <- liftEffect $ window >>= localStorage
    liftEffect $ Storage.setItem authStatusLocalStorageKey
      (stringify $ encodeJson Authorized)
      storage
  setLoggedOut = do
    asks getAuthStatusRef >>= Ref.write Anonymous >>> liftEffect
    storage <- liftEffect $ window >>= localStorage
    liftEffect $ Storage.setItem authStatusLocalStorageKey
      (stringify $ encodeJson Anonymous)
      storage

-- PAGE DATA CHANGES

-- | Has access to halogen-subscription that emits messages when the data for
-- | the current page has been fetched.
class HasPageDataSubscription a where
  pageDataEmitter :: a -> Emitter Unit
  pageDataNotifier :: a -> Listener Unit

instance hasPageDataSubscriptionAppEnv :: HasPageDataSubscription AppEnv where
  pageDataEmitter (Env e) = e.pageDataSub.emitter
  pageDataNotifier (Env e) = e.pageDataSub.listener

-- | Opaque type that represents an async thread that will eventually emit a
-- | page data loaded notification, even if the data has not been loaded. This
-- | allows us to _eventually_ show a loading spinner instead of only changing
-- | the page once data loading is complete.
newtype PageDataDelayedNotif = PageDataDelayedNotif (Fiber Unit)

class Monad m <= PageDataListener m where
  -- | Return an emitter that fires off the given action when a subscription
  -- message is received. Use with 'subscribe'.
  pageDataActionEmitter :: forall a. a -> m (Emitter a)
  -- | Fires a subscription message after the given number of milliseconds
  notifyPageDataAfterMs :: Milliseconds -> m PageDataDelayedNotif
  -- | Kill the thread that fires a message after a delay.
  killDelayedPageDataNotif :: PageDataDelayedNotif -> m Unit

instance pageDataListenerHasPageDataSubscriptionEff ::
  ( HasPageDataSubscription env
  , MonadAsk env m
  , MonadEffect m
  , MonadAff m
  ) =>
  PageDataListener m where
  pageDataActionEmitter action =
    (action <$ _) <$> asks pageDataEmitter
  notifyPageDataAfterMs ms = do
    notifier <- asks pageDataNotifier
    map PageDataDelayedNotif <<< liftAff $ forkAff $ do
      delay ms
      liftEffect $ notify notifier unit
  killDelayedPageDataNotif (PageDataDelayedNotif fiber) =
    liftAff $ killFiber (error "Manually Killed PageDataNotifier Fiber") fiber

-- | Notifies listeners when a page has finished loading it's data.
class Monad m <= PageDataNotifier m where
  -- | Send a message through the page-data-loaded subscription.
  pageDataReceived :: m Unit

instance pageDataNotifierHasPageDataSubscriptionEff ::
  ( HasPageDataSubscription env
  , MonadAsk env m
  , MonadEffect m
  ) =>
  PageDataNotifier m where
  pageDataReceived =
    asks pageDataNotifier >>= flip notify unit >>> liftEffect

-- | Build a Halogen eval function from an eval config. Wrap the 'handleAction'
-- | function in the config & fire off a page data subscription message when the
-- | 'apiData' field has changed from the NotAsked/Loading state to a
-- | Success/Failure state.
-- |
-- | TODO: Maybe we want a new 'Page' module & type with an associated function
-- | that wraps 'mkComponent'. We could then move this into that module and do
-- | any future "Page" related hook ups into there. E.g., setting page title &
-- | metadata. That may let us remove the 'PageDataNotifier' typeclass from the
-- | individual page components as well.
mkPageDataNotifierEval
  :: forall state query action slots input output m e resp a
   . PageDataNotifier m
  => EvalSpec { apiData :: RemoteData e resp | state } query action slots input
       output
       m
  -> H.HalogenQ query action input a
  -> H.HalogenM { apiData :: RemoteData e resp | state } action slots output m a
mkPageDataNotifierEval evalCfg =
  H.mkEval evalCfg { handleAction = interceptEval evalCfg.handleAction }
  where
  -- Check apiData field for NotAsked/Loading -> Success/Failure changes. Call
  -- pageDataReceived if so.
  interceptEval handler action = do
    initialState <- H.gets _.apiData
    handler action
    finalState <- H.gets _.apiData
    let
      finishedLoading = (isNotAsked initialState || isLoading initialState) &&
        (isSuccess finalState || isFailure finalState)
    when finishedLoading $ H.lift pageDataReceived
