{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Api.Routes where

import           Data.Aeson                     ( FromJSON )
import           Data.Text                      ( Text )
import           Servant.API                    ( (:<|>)(..)
                                                , (:>)
                                                , Capture
                                                , Get
                                                , JSON
                                                , MimeUnrender(..)
                                                , NoContent
                                                , Post
                                                , ReqBody
                                                )
import           Servant.API.ContentTypes       ( eitherDecodeLenient )
import           Servant.Auth.Server            ( Auth
                                                , AuthResult(Authenticated)
                                                , Cookie
                                                , JWT
                                                , ThrowAll(throwAll)
                                                )
import           Servant.Auth.Server.Internal.AddSetCookie
                                                ( AddSetCookiesApi
                                                , Nat(..)
                                                )
import           Servant.Docs                   ( DocCapture(..)
                                                , ExtraInfo
                                                , ToCapture(..)
                                                , ToSample(..)
                                                )
import           Servant.Docs.Internal.Pretty   ( Pretty
                                                , PrettyJSON
                                                )
import           Servant.Server                 ( ServerT
                                                , err403
                                                , errBody
                                                )
import           Web.Cookie                     ( SetCookie
                                                , defaultSetCookie
                                                , setCookieName
                                                )

import           App                            ( App )
import qualified Data.ByteString.Lazy.Char8    as LBC
import           Handlers.Admin
import           Handlers.BlogPosts
import           Handlers.Login
import           Models.DB
import           Utils                          ( mkEndpointNotes )


type ServerAPI
    = BlogAPI :<|> LoginAPI :<|> Auth '[Cookie, JWT] UserId :> AdminAPI

api :: ServerT ServerAPI App
api = blogApi :<|> loginApi :<|> adminApi

apiEndpointDocs :: ExtraInfo (Pretty ServerAPI)
apiEndpointDocs = blogNotes <> loginNotes


type LoginAPI
    = AddSetCookiesApi
          ( 'S ( 'S 'Z))
          ("login" :> ReqBody '[JSON] UserLogin :> Post '[JSON] NoContent)

loginApi :: ServerT LoginAPI App
loginApi = userLogin

loginNotes :: ExtraInfo (Pretty ServerAPI)
loginNotes = mkEndpointNotes @LoginAPI @ServerAPI
    ( "Throws"
    , [ "* `401` is user does not exist"
      , "* `401` if password is incorrect"
      , "* `401` if server cannot generate cookies"
      ]
    )


type BlogAPI =
         "blog" :> "posts" :> Get '[JSON] BlogPostList
    :<|> "blog" :> "post" :> Capture "slug" Text :> Get '[JSON] BlogPostDetails

blogNotes :: ExtraInfo (Pretty ServerAPI)
blogNotes =
    mkEndpointNotes
        @( "blog" :> "post" :> Capture "slug" Text :> Get '[JSON] BlogPostDetails
        )
        @ServerAPI
        ( "Throws"
        , [ "* `404` if there is no matching published post with the given slug."
          ]
        )

blogApi :: ServerT BlogAPI App
blogApi = getBlogPosts :<|> getBlogPost


type AdminAPI
    = "admin" :> "blog" :> "post" :> ReqBody '[JSON] NewBlogPost :> Post '[JSON] BlogPostId

adminApi :: AuthResult UserId -> ServerT AdminAPI App
adminApi = \case
    Authenticated uid -> createBlogPost uid
    e                 -> throwAll err403 { errBody = LBC.pack $ show e }



-- ORPHANS

instance ToCapture (Capture "slug" Text) where
    toCapture _ = DocCapture "slug" "slugified title of desired entity"

instance FromJSON a => MimeUnrender PrettyJSON a where
    mimeUnrender _ = eitherDecodeLenient

instance ToSample SetCookie where
    toSamples _ =
        [ ("JWT" , defaultSetCookie { setCookieName = "JWT-Cookie" })
        , ("XSRF", defaultSetCookie { setCookieName = "NO-XSRF-TOKEN" })
        ]
