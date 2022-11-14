{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Api.Routes where

import           Data.Aeson                     ( FromJSON )
import           Data.Text                      ( Text )
import           Data.Time                      ( UTCTime(..)
                                                , fromGregorian
                                                )
import           Servant.API                    ( (:<|>)(..)
                                                , (:>)
                                                , Accept(..)
                                                , Capture
                                                , CaptureAll
                                                , Get
                                                , JSON
                                                , MimeRender(..)
                                                , MimeUnrender(..)
                                                , NoContent
                                                , Post
                                                , ReqBody
                                                )
import           Servant.API.ContentTypes       ( eitherDecodeLenient )
import           Servant.Auth.Docs              ( )
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
                                                , singleSample
                                                )
import           Servant.Docs.Internal.Pretty   ( Pretty
                                                , PrettyJSON
                                                )
import           Servant.Server                 ( ServerT
                                                , err403
                                                )
import           Web.Cookie                     ( SetCookie
                                                , defaultSetCookie
                                                , setCookieName
                                                )
import           Web.Sitemap.Gen                ( ChangeFrequency(Daily)
                                                , Sitemap(..)
                                                , SitemapUrl(..)
                                                , renderSitemap
                                                )

import           App                            ( App )
import           Handlers.Admin
import           Handlers.BlogPosts
import           Handlers.Login
import           Handlers.Sitemap
import           Models.DB
import           Utils                          ( mkEndpointNotes )

import qualified Network.HTTP.Media            as Media


type ServerAPI =
         BlogAPI
    :<|> LoginAPI
    :<|> SitemapAPI
    :<|> Auth '[Cookie, JWT] UserId :> AdminAPI

api :: ServerT ServerAPI App
api = blogApi :<|> loginApi :<|> sitemapApi :<|> adminApi

apiEndpointDocs :: ExtraInfo (Pretty ServerAPI)
apiEndpointDocs = blogNotes <> loginNotes <> adminNotes


-- LOGIN

type LoginAPI
    = AddSetCookiesApi
          ( 'S ( 'S 'Z))
          (     "login" :> ReqBody '[JSON] UserLogin :> Post '[JSON] NoContent
           :<|> "logout" :> Post '[JSON] NoContent
          )

loginApi :: ServerT LoginAPI App
loginApi = userLogin :<|> userLogout

loginNotes :: ExtraInfo (Pretty ServerAPI)
loginNotes =
    mkEndpointNotes
        @( AddSetCookiesApi
              ( 'S ( 'S 'Z))
              ("login" :> ReqBody '[JSON] UserLogin :> Post '[JSON] NoContent)
        )
        @ServerAPI
        ( "Throws"
        , [ "* `401` is user does not exist"
          , "* `401` if password is incorrect"
          , "* `401` if server cannot generate cookies"
          ]
        )


-- SITEMAP

type SitemapAPI =
         "sitemap.xml" :> Get '[XML] Sitemap

sitemapApi :: ServerT SitemapAPI App
sitemapApi = generateSitemap

-- TODO: Extract into @sitemap-gen-servant@ package w/ `ToSample Sitemap`
data XML

instance Accept XML where
    contentType _ = "application" Media.// "xml" Media./: ("charset", "utf-8")

instance MimeRender XML Sitemap where
    mimeRender _ = renderSitemap


-- BLOG

type BlogAPI =
         "blog" :> "posts" :> Get '[JSON] BlogPostList
    :<|> "blog" :> "posts" :> "archive" :> Capture "year" Integer :> Capture "month" Int :> Get '[JSON] BlogPostList
    :<|> "blog" :> "posts" :> "tag" :> Capture "tagSlug" Text :> Get '[JSON] BlogPostList
    :<|> "blog" :> "posts" :> "category" :> Capture "categorySlug" Text :> Get '[JSON] BlogPostList
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
        :<|> getBlogPostsForCategory
        :<|> getBlogPost


-- ADMIN

type AdminAPI =
         "admin" :> "blog" :> "post" :> ReqBody '[JSON] NewBlogPost :> Post '[JSON] BlogPostId
    :<|> "admin" :> "blog" :> "posts" :> Get '[JSON] AdminBlogPostList
    :<|> "admin" :> "blog" :> "post" :> Capture "blogPostId" BlogPostId :> Get '[JSON] AdminBlogPost
    :<|> "admin" :> "blog" :> "post" :> Capture "blogPostId" BlogPostId :> ReqBody '[JSON] AdminBlogPostUpdate :> Post '[JSON] NoContent
    :<|> "admin" :> "blog" :> "categories" :> Get '[JSON] [AdminBlogCategory]
    :<|> "admin" :> "media" :> "list" :> CaptureAll "folderPath" FilePath :> Get '[JSON] AdminMediaList
    :<|> "admin" :> "media" :> "folder" :> CaptureAll "folderPath" FilePath :> Post '[JSON] NoContent
    :<|> "admin" :> "media" :> "upload" :> ReqBody '[JSON] MediaUpload :> Post '[JSON] NoContent

adminApi :: AuthResult UserId -> ServerT AdminAPI App
adminApi = \case
    Authenticated uid ->
        createBlogPost uid
            :<|> getBlogPostsAdmin uid
            :<|> getBlogPostAdmin uid
            :<|> updateBlogPost uid
            :<|> getBlogCategoriesAdmin uid
            :<|> listMediaDirectory uid
            :<|> createMediaDirectory uid
            :<|> uploadMediaFile uid
    _ -> throwAll err403

adminNotes :: ExtraInfo (Pretty ServerAPI)
adminNotes = mconcat
    [ mkEndpointNotes
        @( Auth '[Cookie, JWT] UserId :> "admin" :> "blog" :> "post" :> Capture "blogPostId" BlogPostId :> Get '[JSON] AdminBlogPost
        )
        @ServerAPI
        ("Throws", ["* `404` if there is no matching post with the given ID."])
    , mkEndpointNotes
        @( Auth '[Cookie, JWT] UserId :> "admin" :> "blog" :> "post" :> Capture "blogPostId" BlogPostId :> ReqBody '[JSON] AdminBlogPostUpdate :> Post '[JSON] NoContent
        )
        @ServerAPI
        ("Throws", ["* `404` if there is no matching post with the given ID."])
    ]


-- ORPHANS

instance ToCapture (Capture "postSlug" Text) where
    toCapture _ = DocCapture "postSlug" "slug field of a BlogPost"

instance ToCapture (Capture "blogPostId" BlogPostId) where
    toCapture _ = DocCapture "blogPostId" "ID of a BlogPost"

instance ToCapture (Capture "tagSlug" Text) where
    toCapture _ = DocCapture
        "tagSlug"
        "lowercased BlogPost tag with spaces replaced with hyphens"

instance ToCapture (Capture "categorySlug" Text) where
    toCapture _ = DocCapture "categorySlug" "slug field of a BlogCategory"

instance ToCapture (Capture "year" Integer) where
    toCapture _ = DocCapture "year" "a four-digit year as an integer"

instance ToCapture (Capture "month" Int) where
    toCapture _ = DocCapture "month" "a month by it's number"

instance ToCapture (CaptureAll "folderPath" FilePath) where
    toCapture _ = DocCapture "folderPath" "relative path in media folder"

-- TODO: MR to upstream servant-docs repo?
instance FromJSON a => MimeUnrender PrettyJSON a where
    mimeUnrender _ = eitherDecodeLenient

-- TODO: MR to upstream servant-docs repo?
instance ToSample SetCookie where
    toSamples _ =
        [ ("JWT" , defaultSetCookie { setCookieName = "JWT-Cookie" })
        , ("XSRF", defaultSetCookie { setCookieName = "NO-XSRF-TOKEN" })
        ]

instance ToSample Sitemap where
    toSamples _ = singleSample $ Sitemap
        [ SitemapUrl "https://sleepanarchy.com/"
                     (Just $ UTCTime (fromGregorian 2022 04 20) 0)
                     (Just Daily)
                     (Just 1.0)
        , SitemapUrl "https://sleepanarchy.com/post/my-post"
                     (Just $ UTCTime (fromGregorian 2021 03 19) 0)
                     Nothing
                     Nothing
        ]
