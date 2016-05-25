port module Page.PreviewDocumentation exposing (..) -- where

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Json.Decode as Json
import Task

import Component.Header as Header
import Component.PackagePreview as Preview
import Docs.Package as Docs
import Route



-- WIRES


main =
  Html.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> docsUploads
    }

port uploads : (String -> msg) -> Sub msg

docsUploads : Sub Msg
docsUploads =
  let
    toDocs body =
      case Json.decodeString Docs.decodePackage body of
        Err _ ->
          Preview.Fail (Just "Could not parse file contents as Elm docs.")

        Ok dict ->
          Preview.LoadDocs dict
  in
    Sub.map UpdatePreview (uploads toDocs)

-- MODEL


type alias Model =
    { header : Header.Model
    , preview : Preview.Model
    }

type alias Flags =
    { uploads : String
    }

-- INIT


init : Flags -> (Model, Cmd Msg)
init {uploads} =
  let
    (header, headerCmd) =
      Header.init Route.Help

    (preview, previewCmd) =
      Preview.init
  in
    ( Model header preview
    , Cmd.batch
        [ headerCmd
        , Cmd.map UpdatePreview previewCmd
        ]
    )



-- UPDATE


type Msg
    = UpdatePreview Preview.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdatePreview act ->
        let
          (newPreview, cmd) =
            Preview.update act model.preview
        in
          ( { model | preview = newPreview }
          , Cmd.map UpdatePreview cmd
          )



-- VIEW


view : Model -> Html Msg
view model =
  Html.map UpdatePreview
    (Header.view model.header (Preview.view model.preview))

