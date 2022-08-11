{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Api.Routes where

import           Data.Text                      ( Text )
import           Servant.API                    ( (:<|>)(..)
                                                , (:>)
                                                , Capture
                                                , Get
                                                , JSON
                                                )
import           Servant.Docs                   ( DocCapture(..)
                                                , ExtraInfo
                                                , ToCapture(..)
                                                )
import           Servant.Docs.Internal.Pretty   ( Pretty )
import           Servant.Server                 ( ServerT )

import           App                            ( App )
import           Handlers.BlogPosts
import           Utils                          ( mkEndpointNotes )


type ServerAPI = BlogAPI

api :: ServerT ServerAPI App
api = blogApi

apiEndpointDocs :: ExtraInfo (Pretty ServerAPI)
apiEndpointDocs = blogNotes



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



instance ToCapture (Capture "slug" Text) where
    toCapture _ = DocCapture "slug" "slugified title of desired entity"
