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


-- LOGIN

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


-- BLOG

type BlogAPI =
         "blog" :> "posts" :> Get '[JSON] BlogPostList
    :<|> "blog" :> "posts" :> "archive" :> Capture "year" Integer :> Capture "month" Int :> Get '[JSON] BlogPostList
    :<|> "blog" :> "posts" :> "tag" :> Capture "tagSlug" Text :> Get '[JSON] BlogPostList
    :<|> "blog" :> "post" :> Capture "postSlug" Text :> Get '[JSON] BlogPostDetails

blogNotes :: ExtraInfo (Pretty ServerAPI)
blogNotes =
    mkEndpointNotes
        @( "blog" :> "post" :> Capture "postSlug" Text :> Get '[JSON] BlogPostDetails
        )
        @ServerAPI
        ( "Throws"
        , [ "* `404` if there is no matching published post with the given slug."
          ]
        )

blogApi :: ServerT BlogAPI App
blogApi =
    getBlogPosts
        :<|> getBlogPostsArchive
        :<|> getBlogPostsForTag
        :<|> getBlogPost


-- ADMIN

type AdminAPI
    = "admin" :> "blog" :> "post" :> ReqBody '[JSON] NewBlogPost :> Post '[JSON] BlogPostId

adminApi :: AuthResult UserId -> ServerT AdminAPI App
adminApi = \case
    Authenticated uid -> createBlogPost uid
    e                 -> throwAll err403 { errBody = LBC.pack $ show e }



-- ORPHANS

instance ToCapture (Capture "postSlug" Text) where
    toCapture _ = DocCapture "postSlug" "slug field of a BlogPost"

instance ToCapture (Capture "tagSlug" Text) where
    toCapture _ = DocCapture
        "tagSlug"
        "lowercased BlogPost tag with spaces replaced with hyphens"

instance ToCapture (Capture "year" Integer) where
    toCapture _ = DocCapture "year" "a four-digit year as an integer"

instance ToCapture (Capture "month" Int) where
    toCapture _ = DocCapture "month" "a month by it's number"

-- TODO: MR to upstream servant-docs repo?
instance FromJSON a => MimeUnrender PrettyJSON a where
    mimeUnrender _ = eitherDecodeLenient

-- TODO: MR to upstream servant-docs repo?
instance ToSample SetCookie where
    toSamples _ =
        [ ("JWT" , defaultSetCookie { setCookieName = "JWT-Cookie" })
        , ("XSRF", defaultSetCookie { setCookieName = "NO-XSRF-TOKEN" })
        ]
