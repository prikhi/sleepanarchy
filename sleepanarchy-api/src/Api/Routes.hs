module Api.Routes where

import           Servant.API
import           Servant.Server

import           App
import           Handlers.BlogPosts


type ServerAPI = BlogAPI

api :: ServerT ServerAPI App
api = blogApi


type BlogAPI =
    "blog" :> "posts" :> Get '[JSON] BlogPostList

blogApi :: ServerT BlogAPI App
blogApi = getBlogPosts
