module Api where

import           Control.Monad.Reader           ( ReaderT(..) )
import           Data.Data                      ( Proxy(..) )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Servant.API
import           Servant.Docs
import           Servant.Docs.Internal.Pretty
import           Servant.Server                 ( Application
                                                , Server
                                                , hoistServer
                                                , serve
                                                )

import           Api.Routes
import           App                            ( Config
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
apiServer cfg = hoistServer appApi readerToHandler api
    :<|> return (pack $ markdown apiDocs)
    where readerToHandler = flip runReaderT cfg . runApp

app :: Config -> Application
app cfg = serve appApiWithDocs (apiServer cfg)
