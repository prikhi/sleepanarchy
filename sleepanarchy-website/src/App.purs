module App
  ( AppM
  , runAppM
  , AppEnv(..)
  , class HasNav
  , getNav
  , class Navigation
  , newUrl
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
  , class HasAuthStatus
  , getAuthStatusRef
  , class Auth
  , isLoggedIn
  , setLoggedIn
  , setLoggedOut
  , AuthStatus(..)
  , initializeAuthStatus
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
import Data.Traversable (for)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Now (nowDate)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Foreign (unsafeToForeign)
import Highlight as Highlight
import MarkdownIt (MarkdownIt)
import MarkdownIt as Markdown
import Router (Route, reverse)
import Routing.PushState (PushStateInterface)
import Web.Event.Event (preventDefault)
import Web.File.File (File, toBlob)
import Web.File.FileReader.Aff (readAsDataURL)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
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

instance monadEffectFileUpload :: MonadAff m => FileUpload m where
  encodeBase64 file = liftAff $ do
    dataUrl <- readAsDataURL $ toBlob file
    pure $ String.drop 1 $ String.dropWhile (_ /= String.codePointFromChar ',')
      dataUrl

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
