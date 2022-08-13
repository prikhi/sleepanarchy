module Api where

import           Control.Monad.Reader           ( ReaderT(..) )
import           Data.Data                      ( Proxy(..) )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Servant.API
import           Servant.Auth.Docs              ( )
import           Servant.Auth.Server            ( CookieSettings
                                                , JWTSettings
                                                , cookieXsrfSetting
                                                , defaultCookieSettings
                                                , defaultJWTSettings
                                                )
import           Servant.Docs
import           Servant.Docs.Internal.Pretty
import           Servant.Server                 ( Application
                                                , Context(..)
                                                , Server
                                                , hoistServerWithContext
                                                , serveWithContext
                                                )

import           Api.Routes
import           App                            ( Config(cfgJwk)
                                                , runApp
                                                )

appApi :: Proxy (Pretty ServerAPI)
appApi = pretty (Proxy @ServerAPI)

apiDocs :: API
apiDocs = docsWith defaultDocOptions [] apiEndpointDocs appApi

type DocsAPI = "docs" :> Get '[PlainText] Text

appApiWithDocs :: Proxy (ServerAPI :<|> DocsAPI)
appApiWithDocs = Proxy

apiServer :: Config -> Server (ServerAPI :<|> DocsAPI)
apiServer cfg =
    hoistServerWithContext appApi
                           (Proxy @'[CookieSettings , JWTSettings])
                           readerToHandler
                           api
        :<|> return (pack $ markdown apiDocs)
    where readerToHandler = flip runReaderT cfg . runApp

app :: Config -> Application
app cfg = serveWithContext
    appApiWithDocs
    (  defaultCookieSettings { cookieXsrfSetting = Nothing }
    :. defaultJWTSettings (cfgJwk cfg)
    :. EmptyContext
    )
    (apiServer cfg)
