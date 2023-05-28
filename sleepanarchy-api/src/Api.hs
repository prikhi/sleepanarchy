module Api where

import Control.Monad.Reader (ReaderT (..))
import Data.Data (Proxy (..))
import Data.Text (Text, pack)
import Servant.API
import Servant.Auth.Docs ()
import Servant.Auth.Server (CookieSettings, JWTSettings)
import Servant.Docs
import Servant.Docs.Internal.Pretty
import Servant.Server
    ( Application
    , Context (..)
    , Handler
    , Server
    , hoistServerWithContext
    , serveWithContext
    )
import Servant.Server.StaticFiles (serveDirectoryWebApp)

import Api.Routes
import App
    ( App
    , AuthToken (..)
    , Config (..)
    , Environment (..)
    , runApp
    )


appApi :: Proxy (Pretty ServerAPI)
appApi = pretty (Proxy @ServerAPI)


apiDocs :: API
apiDocs = docsWith defaultDocOptions [] apiEndpointDocs appApi


type DocsAPI = "docs" :> Get '[PlainText] Text


appApiWithDocs :: Proxy (ServerAPI :<|> DocsAPI)
appApiWithDocs = Proxy


devApi :: Proxy ((ServerAPI :<|> DocsAPI) :<|> "media" :> Raw)
devApi = Proxy


apiServer :: Config -> Server (ServerAPI :<|> DocsAPI)
apiServer cfg =
    hoistServerWithContext
        appApi
        (Proxy @'[CookieSettings, JWTSettings])
        appToHandler
        api
        :<|> return (pack $ markdown apiDocs)
  where
    appToHandler :: App a -> Handler a
    appToHandler = flip runReaderT cfg . runApp


app :: Config -> Application
app cfg =
    let context = (getCookieSettings cfg :. getJWTSettings cfg :. EmptyContext)
        server = apiServer cfg
     in case cfgEnv cfg of
            Production ->
                serveWithContext appApiWithDocs context server
            Development ->
                serveWithContext devApi context $
                    server
                        :<|> serveDirectoryWebApp (cfgMediaDirectory cfg)
