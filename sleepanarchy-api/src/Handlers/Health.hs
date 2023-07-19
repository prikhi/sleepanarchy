module Handlers.Health where

import Servant (NoContent (..))

import App (Cache, DB (..))
import Handlers.BlogPosts (getBlogPosts)


healthcheck :: (Cache m, DB m, Monad m) => m NoContent
healthcheck =
    getBlogPosts >> return NoContent
