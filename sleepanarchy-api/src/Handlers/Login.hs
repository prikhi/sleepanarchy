{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MonoLocalBinds #-}
module Handlers.Login where

import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                )
import           Data.Password.Argon2           ( Argon2
                                                , PasswordCheck(..)
                                                , PasswordHash(..)
                                                , checkPassword
                                                , mkPassword
                                                )
import           Data.Text                      ( Text )
import           Database.Persist               ( Entity(..)
                                                , getBy
                                                )
import           GHC.Generics                   ( Generic )
import           Servant                        ( Header
                                                , Headers
                                                , NoContent(..)
                                                , err401
                                                )
import           Servant.Auth.Server            ( SetCookie
                                                , acceptLogin
                                                , cookieXsrfSetting
                                                , defaultCookieSettings
                                                )
import           Servant.Docs                   ( ToSample(..)
                                                , singleSample
                                                )

import           App                            ( DB(..)
                                                , JWTToken(..)
                                                , ThrowsError(..)
                                                )
import           Models.DB
import           Utils                          ( prefixParseJSON
                                                , prefixToJSON
                                                )


data UserLogin = UserLogin
    { ulName     :: Text
    , ulPassword :: Text
    }
    deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON UserLogin where
    toJSON = prefixToJSON "ul"

instance FromJSON UserLogin where
    parseJSON = prefixParseJSON "ul"

instance ToSample UserLogin where
    toSamples _ = singleSample $ UserLogin "myUserName" "hunter123"

userLogin
    :: (MonadIO m, DB m, JWTToken m, ThrowsError m)
    => UserLogin
    -> m
           ( Headers
                 '[ Header "Set-Cookie" SetCookie
                  , Header "Set-Cookie" SetCookie
                  ]
                 NoContent
           )
userLogin UserLogin {..} = do
    runDB (getBy (UniqueUser ulName)) >>= \case
        Nothing                -> serverError err401
        Just (Entity uid user) -> do
            let verificationResult = checkPassword
                    (mkPassword ulPassword)
                    (PasswordHash @Argon2 $ userPassword user)
            case verificationResult of
                PasswordCheckSuccess -> do
                    jwtSettings   <- getJWTSettings
                    mApplyCookies <- liftIO $ acceptLogin
                        defaultCookieSettings { cookieXsrfSetting = Nothing }
                        jwtSettings
                        uid
                    case mApplyCookies of
                        Nothing           -> serverError err401
                        Just applyCookies -> return $ applyCookies NoContent
                PasswordCheckFail -> serverError err401
