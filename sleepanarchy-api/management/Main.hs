module Main where

import Control.Exception.Safe (try)
import Control.Monad.Reader (liftIO, runReaderT)
import Data.Aeson (encode)
import Data.Password.Argon2
    ( Password
    , PasswordHash (..)
    , hashPassword
    , mkPassword
    )
import Database.Persist.Sql (BackendKey (..), insert)
import Database.PostgreSQL.Simple (SqlError)
import Servant.Auth.Server (generateKey)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import App
import Models.DB

import Data.Text qualified as T (pack)


main :: IO ()
main =
    getArgs >>= \case
        ["create-user", name, password] ->
            createUser name (mkPassword $ T.pack password)
        ["generate-jwk"] ->
            generateJWK
        _ ->
            mapM_
                (hPutStrLn stderr)
                [ "sleepanarchy-api-management: Helper Commands for Managing the Sleepanarchy.com API"
                , ""
                , "create-user <user-name> <password>       Add an admin user with the given credentials."
                , "generate-jwk                             Create a JWK for token signing."
                , ""
                ]
                >> exitFailure


createUser :: String -> Password -> IO ()
createUser name password = do
    (PasswordHash hashedPass) <- hashPassword password
    cfg <- mkConfig
    flip runReaderT cfg $ do
        mbUserId <-
            try @_ @SqlError . runDB . insert $
                User
                    (T.pack name)
                    hashedPass
        liftIO $ case mbUserId of
            Right userId ->
                putStrLn $
                    "Successfully inserted user '"
                        <> name
                        <> "' with ID #"
                        <> show (unSqlBackendKey (unUserKey userId))
            Left _ -> do
                putStrLn $ "Could not insert user, name is taken: " <> name
                exitFailure


generateJWK :: IO ()
generateJWK = do
    jwk <- generateKey
    putStrLn
        "Generated JWK. Set `API_JWK` to this value for use with the API server:"
    putStr "    "
    print $ encode jwk
