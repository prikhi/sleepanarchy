{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Exception.Safe (try)
import Control.Monad (forM_)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.Reader (liftIO, runReaderT)
import Control.Monad.Trans (lift)
import Data.Aeson (encode)
import Data.Maybe (catMaybes)
import Data.Password.Argon2
    ( Password
    , PasswordHash (..)
    , hashPassword
    , mkPassword
    )
import Database.Persist.Sql
    ( BackendKey (..)
    , Entity (..)
    , SqlPersistT
    , insert
    , selectList
    , update
    , (=.)
    )
import Database.PostgreSQL.Simple (SqlError)
import Servant.Auth.Server (generateKey)
import Skylighting.Types
    ( Color
    , Style (..)
    , TokenStyle (..)
    , TokenType (..)
    , defStyle
    , toColor
    )
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Pandoc (PandocError, renderError)
import Text.Pandoc.Highlighting (styleToCss)

import App
import Models.DB
import Utils (renderMarkdown)

import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as T


main :: IO ()
main =
    getArgs >>= \case
        ["create-user", name, password] ->
            createUser name (mkPassword $ T.pack password)
        ["generate-jwk"] ->
            generateJWK
        ["regenerate-html"] ->
            regenerateHtml
        ["print-syntax-css"] ->
            printSyntaxCss
        _ ->
            mapM_
                (hPutStrLn stderr)
                [ "sleepanarchy-api-management: Helper Commands for Managing the Sleepanarchy.com API"
                , ""
                , "create-user <user-name> <password>       Add an admin user with the given credentials."
                , "generate-jwk                             Create a JWK for token signing."
                , "regenerate-html                          Regenerate the post body & description HTML."
                , "print-syntax-css                         Output syntax highlighting CSS."
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


regenerateHtml :: IO ()
regenerateHtml = do
    cfg <- mkConfig
    flip runReaderT cfg $ runDB $ do
        posts <- selectList [] []
        forM_ posts $ \(Entity pId BlogPost {..}) -> do
            result <- runExceptT $ do
                contentHtml <- render blogPostContent
                descriptionHtml <- render blogPostDescription
                lift $
                    update
                        pId
                        [ BlogPostContentHtml =. contentHtml
                        , BlogPostDescriptionHtml =. descriptionHtml
                        ]
            case result of
                Right _ -> return ()
                Left e ->
                    liftIO $ T.putStrLn $ "Error rendering markdown: " <> renderError e
  where
    render :: T.Text -> ExceptT PandocError (SqlPersistT IO) T.Text
    render = ExceptT . return . renderMarkdown


printSyntaxCss :: IO ()
printSyntaxCss =
    putStrLn $ prefix <> styleToCss Style {..}
  where
    prefix :: String
    prefix = "/** Generated via `sleepanarchy-api-management print-syntax-css` **/\n"
    toColorS :: String -> Maybe Color
    toColorS = toColor
    defaultColor :: Maybe Color
    defaultColor = toColorS "#F8F8F2"
    backgroundColor :: Maybe Color
    backgroundColor = toColorS "#1B1D1E"
    lineNumberColor :: Maybe Color
    lineNumberColor = toColorS "#465457"
    lineNumberBackgroundColor :: Maybe Color
    lineNumberBackgroundColor = toColorS "#232526"
    tokenStyles :: M.Map TokenType TokenStyle
    tokenStyles =
        M.fromList $
            catMaybes
                [(t,) <$> tokenStyle t | SkyToken t <- [minBound .. maxBound]]
    color :: String -> Maybe TokenStyle
    color c = Just $ defStyle {tokenColor = toColorS c}
    bg :: String -> Maybe TokenStyle -> Maybe TokenStyle
    bg c = fmap $ \s -> s {tokenBackground = toColorS c}
    bold :: Maybe TokenStyle -> Maybe TokenStyle
    bold = fmap $ \s -> s {tokenBold = True}
    italics :: Maybe TokenStyle -> Maybe TokenStyle
    italics = fmap $ \s -> s {tokenItalic = True}
    tokenStyle :: TokenType -> Maybe TokenStyle
    tokenStyle = \case
        KeywordTok ->
            bold $ color "#F92672"
        DataTypeTok ->
            color "#66D9EF"
        DecValTok ->
            color "#AE81FF"
        BaseNTok ->
            color "#AE81FF"
        FloatTok ->
            color "#AE81FF"
        ConstantTok ->
            color "#AE81FF"
        CharTok ->
            color "#E6DB74"
        SpecialCharTok ->
            color "#F92672"
        StringTok ->
            color "#E6DB74"
        VerbatimStringTok ->
            color "#E6DB74"
        SpecialStringTok ->
            color "#E6DB74"
        ImportTok ->
            bold $ color "#F92672"
        CommentTok ->
            color "#7E8E91"
        DocumentationTok ->
            color "#E6DB74"
        AnnotationTok ->
            color "#A6E22E"
        CommentVarTok ->
            color "#7E8E91"
        OtherTok ->
            Nothing
        FunctionTok ->
            color "#A6E22E"
        VariableTok ->
            color "#FD971F"
        ControlFlowTok ->
            bold $ color "#F92672"
        OperatorTok ->
            color "#F92672"
        BuiltInTok ->
            Nothing
        ExtensionTok ->
            color "#A6E22E"
        PreprocessorTok ->
            italics $ color "#C4BE89"
        AttributeTok ->
            color "#A6E22E"
        RegionMarkerTok ->
            color "#8F8F8F"
        InformationTok ->
            bold $ color "#7E8E91"
        WarningTok ->
            bold $ bg "#333333" $ color "#FFFFFF"
        AlertTok ->
            bold $ color "#FFFFFF"
        ErrorTok ->
            bold $ bg "#232526" $ color "#F92672"
        NormalTok ->
            Nothing


-- | Custom type for deriving "Bounded TokenType".
newtype SkyToken = SkyToken TokenType
    deriving newtype (Enum)


instance Bounded SkyToken where
    minBound = SkyToken KeywordTok
    maxBound = SkyToken NormalTok
