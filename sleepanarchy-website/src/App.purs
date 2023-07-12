module App
  ( AppM
  , runAppM
  , AppEnv(..)
  , class HasNav
  , getNav
  , class Navigation
  , newUrl
  , openInNewTab
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
  , PageLoadData
  , SEOData
  , adminSEOData
  , class HasMetaElements
  , getMetaElements
  , class MetaTags
  , setMetaTags
  , MetaElements
  , mkMetaElements
  ) where

import Prelude

import Affjax.StatusCode (StatusCode(..))
import Affjax.Web (printError)
import Api (ApiError(..))
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
  , printJsonDecodeError
  , stringify
  )
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Date (Date)
import Data.Either (hush)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.Time.Duration (Milliseconds)
import Data.Traversable (for, for_)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, delay, error, forkAff, killFiber)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throwException)
import Effect.Now (nowDate)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign (unsafeToForeign)
import Halogen as H
import Halogen.Component (EvalSpec)
import Halogen.Subscription (Emitter, Listener, SubscribeIO, notify)
import Network.RemoteData (RemoteData(..), isLoading, isNotAsked)
import Router (Route, reverse)
import Routing.PushState (PushStateInterface)
import Web.DOM (Element)
import Web.DOM.Document as Document
import Web.DOM.Element as Element
import Web.DOM.Node (appendChild)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.Event (preventDefault)
import Web.File.File (File, toBlob)
import Web.File.FileReader.Aff (readAsDataURL)
import Web.HTML (window)
import Web.HTML as HTML
import Web.HTML.HTMLDocument as DocumentHTML
import Web.HTML.HTMLElement as ElementHTML
import Web.HTML.HTMLHeadElement as HeadHTML
import Web.HTML.Window (localStorage, open)
import Web.HTML.Window as Window
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
    , authStatus :: Ref AuthStatus
    , pageDataSub :: SubscribeIO (Maybe PageLoadData)
    , metaElements :: MetaElements
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

-- | Full set of metadata for page load events.
type PageLoadData =
  { seoData :: SEOData
  , apiStatusCode :: Int
  }

-- | SEO-specific data for the page as a whole.
type SEOData =
  { pageTitle :: String
  , metaDescription :: String
  }

-- | SEOData constructor for admin pages. Takes a page title & tosses away any
-- | component state or request data.
adminSEOData :: forall a b. String -> a -> b -> SEOData
adminSEOData pageTitle _ _ = { pageTitle, metaDescription: "" }

-- | Has access to halogen-subscription that emits messages when the data for
-- | the current page has been fetched.
class HasPageDataSubscription a where
  pageDataEmitter :: a -> Emitter (Maybe PageLoadData)
  pageDataNotifier :: a -> Listener (Maybe PageLoadData)

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
  -- message is received. Use with 'subscribe'. The 'SEOData' will be present
  -- when the message represents a page load and empty if the message was sent
  -- after the timeout by 'notifyPageDataAfterMs'.
  pageDataActionEmitter :: forall a. (Maybe PageLoadData -> a) -> m (Emitter a)
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
    (action <$> _) <$> asks pageDataEmitter
  notifyPageDataAfterMs ms = do
    notifier <- asks pageDataNotifier
    map PageDataDelayedNotif <<< liftAff $ forkAff $ do
      delay ms
      liftEffect $ notify notifier Nothing
  killDelayedPageDataNotif (PageDataDelayedNotif fiber) =
    liftAff $ killFiber (error "Manually Killed PageDataNotifier Fiber") fiber

-- | Notifies listeners when a page has finished loading it's data.
class Monad m <= PageDataNotifier m where
  -- | Send a message through the page-data-loaded subscription.
  pageDataReceived :: PageLoadData -> m Unit

instance pageDataNotifierHasPageDataSubscriptionEff ::
  ( HasPageDataSubscription env
  , MonadAsk env m
  , MonadEffect m
  ) =>
  PageDataNotifier m where
  pageDataReceived pageData =
    asks pageDataNotifier >>= flip notify (Just pageData) >>> liftEffect

-- | Build a Halogen eval function from an eval config. Wrap the 'handleAction'
-- | function in the config & fire off a page data subscription message when the
-- | 'apiData' field has changed from the NotAsked/Loading state to a
-- | Success/Failure state.
-- |
-- | When the page data request is successful, we use the supplied function to
-- | generate an `SEOData` for the subscription. When an error is received from
-- | the API, we create a standardized `SEOData` with the error & message.
-- |
-- | TODO: Maybe we want a new 'Page' module & type with an associated function
-- | that wraps 'mkComponent'. We could then move this into that module and do
-- | any future "Page" related hook ups into there. E.g., setting page title &
-- | metadata. That may let us remove the 'PageDataNotifier' typeclass from the
-- | individual page components as well.
mkPageDataNotifierEval
  :: forall state query action slots input output m resp a
   . PageDataNotifier m
  => ({ apiData :: RemoteData ApiError resp | state } -> resp -> SEOData)
  -> EvalSpec { apiData :: RemoteData ApiError resp | state } query action slots
       input
       output
       m
  -> H.HalogenQ query action input a
  -> H.HalogenM { apiData :: RemoteData ApiError resp | state } action slots
       output
       m
       a
mkPageDataNotifierEval mkSEOData evalCfg =
  H.mkEval evalCfg { handleAction = interceptEval evalCfg.handleAction }
  where
  -- Check apiData field for NotAsked/Loading -> Success/Failure changes. Call
  -- pageDataReceived if so.
  interceptEval handler action = do
    initialState <- H.gets _.apiData
    handler action
    finalState <- H.get
    let initialLoading = isNotAsked initialState || isLoading initialState
    when initialLoading $ case finalState.apiData of
      Success x -> H.lift $ pageDataReceived
        { seoData: mkSEOData finalState x, apiStatusCode: 200 }
      Failure e -> H.lift $ pageDataReceived $ errorPageData e
      _ -> pure unit

  errorPageData :: ApiError -> PageLoadData
  errorPageData =
    case _ of
      HttpError e ->
        { seoData:
            { pageTitle: "HTTP Error"
            , metaDescription: printError e
            }
        , apiStatusCode: 500
        }
      StatusCodeError { status: StatusCode code, body } ->
        { seoData:
            { pageTitle: show code <> " Error"
            , metaDescription: body
            }
        , apiStatusCode: code
        }
      JsonError e ->
        { seoData:
            { pageTitle: "JSON Decoding Error"
            , metaDescription: printJsonDecodeError e
            }
        , apiStatusCode: 429
        }

-- SEO META TAGS

-- | Pointers to various elements we need to manipulate during runtime for SEO
-- | purposes.
type MetaElements =
  { document :: HTML.HTMLDocument
  , metaDescription :: Element
  , ogDescription :: Element
  , ogSiteName :: Element
  , prerenderStatusCode :: Element
  }

-- | Initialize the MetaElements type by querying for their respective meta
-- | tags in the document head.
-- |
-- | Any missing elements will be created. The ogSiteName will be set to `Sleep
-- | Anarchy` while the other elements will not be modified.
mkMetaElements :: Effect MetaElements
mkMetaElements = do
  document <- window >>= Window.document
  headEl <- DocumentHTML.head document >>= case _ of
    Just h -> pure h
    Nothing -> do
      headEl <- Document.createElement "head" $ DocumentHTML.toDocument document
      appendChild (Element.toNode headEl) (DocumentHTML.toNode document)
      maybe
        (throwException $ error "created head not parsed as HTMLHeadElement")
        (pure <<< HeadHTML.toHTMLElement) $
        HeadHTML.fromElement headEl
  prerenderStatusCode <- findOrCreateMetaElement document headEl "name"
    "prerender-status-code"
  metaDescription <- findOrCreateMetaElement document headEl "name"
    "description"
  ogDescription <- findOrCreateMetaElement document headEl "property"
    "og:description"
  ogSiteName <- findOrCreateMetaElement document headEl "property"
    "og:site_name"
  Element.setAttribute "content" "Sleep Anarchy" ogSiteName
  pure
    { document
    , metaDescription
    , ogDescription
    , ogSiteName
    , prerenderStatusCode
    }
  where
  -- Find the `meta` tag with the given attribute name & value. Create it and
  -- append it to the `head` tag if missing.
  findOrCreateMetaElement
    :: HTML.HTMLDocument
    -> HTML.HTMLElement
    -> String
    -> String
    -> Effect Element
  findOrCreateMetaElement document headEl tag prop = do
    let parent = ElementHTML.toParentNode headEl
    mbEl <- querySelector
      (QuerySelector $ "meta[" <> tag <> "=\"" <> prop <> "\"]")
      parent
    case mbEl of
      Just e -> pure e
      Nothing -> do
        el <- Document.createElement "meta" $ DocumentHTML.toDocument document
        Element.setAttribute tag prop el
        appendChild (Element.toNode el) (ElementHTML.toNode headEl)
        pure el

class HasMetaElements a where
  getMetaElements :: a -> MetaElements

instance hasMetaElementsAppEnv :: HasMetaElements AppEnv where
  getMetaElements (Env e) = e.metaElements

-- | Manipulate the `title` & `meta` HTML tags.
class Monad m <= MetaTags m where
  setMetaTags :: PageLoadData -> m Unit

instance metaTagsHasMetaElementsEff ::
  ( HasMetaElements env
  , MonadAsk env m
  , MonadEffect m
  ) =>
  MetaTags m where
  setMetaTags { apiStatusCode, seoData: { pageTitle, metaDescription } } = do
    meta <- asks getMetaElements
    let
      fullTitle =
        if String.null pageTitle then "Sleep Anarchy"
        else pageTitle <> " | Sleep Anarchy"
    liftEffect $ DocumentHTML.setTitle fullTitle meta.document
    liftEffect $ for_ [ meta.metaDescription, meta.ogDescription ] $ \el ->
      Element.setAttribute "content" metaDescription el
    liftEffect $ Element.setAttribute "content" (show apiStatusCode)
      meta.prerenderStatusCode
