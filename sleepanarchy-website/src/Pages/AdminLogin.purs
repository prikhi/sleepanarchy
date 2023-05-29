module Pages.AdminLogin (page) where

import Prelude

import Affjax.StatusCode (StatusCode(..))
import Api
  ( class ApiRequest
  , ApiError(..)
  , SubmitFormEvent
  , adminLogin
  , onSubmit
  , preventFormSubmission
  , renderApiError
  )
import App (class Auth, class Navigation, newUrl, setLoggedIn)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..), isSuccess)
import Router (Route, parseRedirectPath)

page
  :: forall q o m
   . ApiRequest m
  => Navigation m
  => Auth m
  => H.Component q (Maybe String) o m
page =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

type State =
  { username :: String
  , password :: String
  , response :: RemoteData ApiError Unit
  , redirectTo :: Route
  }

initialState :: Maybe String -> State
initialState mbRedirectPath =
  { username: ""
  , password: ""
  , response: NotAsked
  , redirectTo: parseRedirectPath mbRedirectPath
  }

data Action
  = SetUsername String
  | SetPassword String
  | MakeRequest SubmitFormEvent

handleAction
  :: forall o m
   . ApiRequest m
  => Navigation m
  => Auth m
  => Action
  -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  SetUsername str ->
    H.modify_ _ { username = str }
  SetPassword str ->
    H.modify_ _ { password = str }
  MakeRequest ev -> do
    H.lift $ preventFormSubmission ev
    st <- H.get
    H.modify_ _ { response = Loading }
    response <- H.lift $ adminLogin st.username st.password
    when (isSuccess response) $ do
      H.lift setLoggedIn
      H.lift $ newUrl st.redirectTo Nothing
    H.modify_ _ { response = response }

render :: forall m. State -> H.ComponentHTML Action () m
render st =
  HH.form [ HP.classes [ H.ClassName "login" ], onSubmit MakeRequest ]
    [ HH.h1_ [ HH.text "Sleep Anarchy Admin" ]
    , HH.label_
        [ HH.div_ [ HH.text "Username:" ]
        , HH.input
            [ HP.value st.username
            , HP.type_ HP.InputText
            , HE.onValueInput SetUsername
            ]
        ]
    , HH.label_
        [ HH.div_ [ HH.text "Password:" ]
        , HH.input
            [ HP.value st.password
            , HP.type_ HP.InputPassword
            , HE.onValueInput SetPassword
            ]
        ]
    , HH.button
        [ HP.type_ HP.ButtonSubmit ]
        [ HH.text "Log in" ]
    , errMsg
    ]
  where
  errMsg :: forall w a. HH.HTML w a
  errMsg =
    case st.response of
      Failure err@(StatusCodeError resp) ->
        if resp.status == StatusCode 401 then
          HH.p_ [ HH.text "Incorrect username or password." ]
        else
          HH.p_
            [ HH.text $ "An error occured when trying to log in: " <>
                renderApiError err
            ]
      Failure err ->
        HH.p_
          [ HH.text $ "An error occured when trying to log in: " <>
              renderApiError err
          ]
      _ ->
        HH.text ""
