module Main where

import           Network.Wai.Handler.Warp

import           Api
import           App

main :: IO ()
main = do
    let port = 9001
    putStrLn $ "Starting Server on Port " <> show port
    run port $ app Config