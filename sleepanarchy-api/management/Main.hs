module Main where

import Data.Password.Argon2 (hashPassword, mkPassword, Password, PasswordHash(..))
import Control.Monad.Reader (runReaderT, liftIO)
import Control.Exception.Safe (try)
import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Database.Persist.Sql (insert, BackendKey(..))
import Database.PostgreSQL.Simple (SqlError)

import App
import Models.DB

import qualified Data.Text as T (pack)


main :: IO ()
main = getArgs >>= \case
    ["create-user", name, password] -> createUser name (mkPassword $ T.pack password)
    _ ->
        mapM_ (hPutStrLn stderr)
            [ "sleepanarchy-api-management: Helper Commands for Managing the Sleepanarchy.com API"
            , ""
            , "create-user <user-name> <password>       Add an admin user with the given credentials."
            , ""
            ] >> exitFailure

createUser :: String -> Password -> IO ()
createUser name password = do
    (PasswordHash hashedPass) <- hashPassword password
    cfg <- mkConfig
    flip runReaderT cfg $ do
        mbUserId <- try @_ @SqlError . runDB . insert $ User (T.pack name) hashedPass
        liftIO $ case mbUserId of
            Right userId ->
                putStrLn $ "Successfully inserted user '" <> name <> "' with ID #" <> show (unSqlBackendKey (unUserKey userId))
            Left _ -> do
                putStrLn $ "Could not insert user, name is taken: " <> name
                exitFailure
