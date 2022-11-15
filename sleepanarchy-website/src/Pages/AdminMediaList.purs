module Pages.AdminMediaList (page) where

import Prelude

import Api
  ( class ApiRequest
  , ApiError
  , SubmitFormEvent
  , adminMediaFolderCreateRequest
  , adminMediaListRequest
  , adminMediaUploadRequest
  , onSubmit
  , preventFormSubmission
  , renderApiError
  )
import Api.Types (AdminMediaList, AdminMediaListItem, FileType(..))
import App
  ( class FileUpload
  , class Navigation
  , clearInputValue
  , encodeBase64
  , newUrl
  )
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Router (AdminRoute(..), Route(..), navLinkAttr)
import Views.Forms (mkInput, mkSubmit)
import Web.File.File (File, name)
import Web.UIEvent.MouseEvent as ME

page
  :: forall q o m
   . Navigation m
  => ApiRequest m
  => FileUpload m
  => H.Component q (Array String) o m
page = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction, initialize = Just Initialize }
  }

type State =
  { folderPath :: Array String
  , newFolder :: String
  , fileInputLabel :: H.RefLabel
  , apiData :: Maybe (Either ApiError AdminMediaList)
  , fileUploadResponse :: Maybe (Either ApiError String)
  }

initialState :: Array String -> State
initialState folderPath =
  { folderPath
  , newFolder: ""
  , fileInputLabel: H.RefLabel "file-input"
  , apiData: Nothing
  , fileUploadResponse: Nothing
  }

data Action
  = Initialize
  | SetNewFolder String
  | CreateFolder SubmitFormEvent
  | UploadFile (Maybe File)
  | NavigateTo Route ME.MouseEvent

handleAction
  :: forall o m
   . Navigation m
  => ApiRequest m
  => FileUpload m
  => Action
  -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> requestCurrentPath
  SetNewFolder val ->
    H.modify_ _ { newFolder = val }
  CreateFolder ev -> do
    H.lift $ preventFormSubmission ev
    st <- H.get
    unless (String.null $ st.newFolder) $ do
      -- TODO: Store response, show error
      _ <- H.lift $ adminMediaFolderCreateRequest st.folderPath st.newFolder
      H.lift $ newUrl
        (Admin $ AdminMediaList $ st.folderPath <> [ st.newFolder ])
        Nothing
  UploadFile mbFile -> case mbFile of
    Nothing -> pure unit
    Just file -> do
      folderPath <- H.gets _.folderPath
      let fileName = name file
      encodedFile <- H.lift $ encodeBase64 file
      response <- H.lift $ adminMediaUploadRequest fileName encodedFile
        folderPath
      H.modify_ _ { fileUploadResponse = Just response }
      case response of
        Right _ ->
          H.gets _.fileInputLabel >>= H.getRef >>= traverse_
            (H.lift <<< clearInputValue)
        Left _ -> pure unit
      requestCurrentPath
  NavigateTo route ev ->
    H.lift $ newUrl route $ Just ev
  where
  requestCurrentPath :: H.HalogenM State Action () o m Unit
  requestCurrentPath = do
    folderPath <- H.gets _.folderPath
    response <- H.lift $ adminMediaListRequest folderPath
    H.modify_ _ { apiData = Just response }

render :: forall m. State -> H.ComponentHTML Action () m
render st = case st.apiData of
  Nothing -> HH.div_ [ HH.text "Loading..." ]
  Just (Left e) ->
    HH.div_ [ HH.text $ renderApiError e ]
  Just (Right resp) ->
    HH.div [ HP.classes [ H.ClassName "admin-media-list" ] ]
      [ HH.h1_ [ HH.text "Media" ]
      , HH.h2_ [ pathLinks ]
      , createFolderHtml
      , HH.br_
      , uploadFileHtml
      , HH.ul_ $ map renderItem resp.contents
      ]
    where
    pathLinks :: forall w. HH.HTML w Action
    pathLinks =
      let
        pathParts =
          map (\x -> x { init = Array.filter (not <<< String.null) x.init })
            $ inits
            $ String.split (String.Pattern "/")
            $ fromMaybe resp.basePath
            $ String.stripSuffix (String.Pattern "/") resp.basePath
        mkLink { init, last } =
          let
            Tuple name folders =
              if String.null last then Tuple "media" []
              else Tuple last (Array.snoc init last)
          in
            HH.a (navLinkAttr NavigateTo $ Admin $ AdminMediaList folders)
              [ HH.text name ]
      in
        HH.div_ $ Array.cons (HH.text "/ ") $ Array.intersperse (HH.text " / ")
          $ map mkLink pathParts

    inits :: forall a. Array a -> Array ({ init :: Array a, last :: a })
    inits = Array.unsnoc >>> case _ of
      Nothing -> []
      Just r@{ init } -> inits init <> [ r ]

    createFolderHtml :: forall w. HH.HTML w Action
    createFolderHtml =
      HH.form [ onSubmit CreateFolder ]
        [ mkInput st unit "Folder Name" Nothing _.newFolder (const Nothing)
            SetNewFolder
        , mkSubmit "Create"
        ]

    uploadFileHtml :: forall w. HH.HTML w Action
    uploadFileHtml =
      HH.label_
        [ HH.text "Upload File "
        , HH.input
            [ HP.type_ HP.InputFile
            , HE.onFileUpload UploadFile
            , HP.ref st.fileInputLabel
            ]
        , HH.p_
            [ case st.fileUploadResponse of
                Nothing -> HH.text ""
                Just (Left e) -> HH.text $ "Error uploading file: " <>
                  renderApiError e
                Just (Right fn) -> HH.span_
                  [ HH.text "Successfully uploaded ", mkFileLink fn ]
            ]
        ]

    renderItem :: forall w. AdminMediaListItem -> HH.HTML w Action
    renderItem { fileType, name } =
      if fileType == Directory then
        HH.li_
          [ HH.a
              ( navLinkAttr NavigateTo $ Admin $ AdminMediaList $
                  st.folderPath <> [ name ]
              )
              [ HH.text $ name <> "/" ]
          ]
      else
        HH.li_ [ mkFileLink name ]

    mkFileLink :: forall w i. String -> HH.HTML w i
    mkFileLink name = HH.a
      [ HP.href $ "/media/" <> String.joinWith "/" st.folderPath <> "/"
          <> name
      , HP.target "_blank"
      ]
      [ HH.text name ]
