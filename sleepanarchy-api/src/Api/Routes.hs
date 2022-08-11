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
import           Servant.Server                 ( ServerT )
import           Web.Cookie                     ( SetCookie
                                                , defaultSetCookie
                                                , setCookieName
                                                )

import           App                            ( App )
import           Handlers.BlogPosts
import           Handlers.Login
import           Utils                          ( mkEndpointNotes )


type ServerAPI = BlogAPI :<|> LoginAPI

api :: ServerT ServerAPI App
api = blogApi :<|> loginApi

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



-- ORPHANS

instance ToCapture (Capture "slug" Text) where
    toCapture _ = DocCapture "slug" "slugified title of desired entity"

instance FromJSON a => MimeUnrender PrettyJSON a where
    mimeUnrender _ = eitherDecodeLenient

instance ToSample SetCookie where
    toSamples _ =
        [ ("JWT" , defaultSetCookie { setCookieName = "JWT-Cookie" })
        , ("XSRF", defaultSetCookie { setCookieName = "XSRF-TOKEN" })
        ]
