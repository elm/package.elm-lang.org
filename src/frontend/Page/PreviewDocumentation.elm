port module Page.PreviewDocumentation exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Json
import Task

import Component.Header as Header
import Component.PackagePreview as Preview
import Docs.Package as Docs
import Route



main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
    { header : Header.Model
    , preview : Preview.Model
    }



-- INIT


init : (Model, Cmd Msg)
init =
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



-- SUBSCRIPTIONS


port uploads : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
  uploads (UpdatePreview << toDocs)


toDocs : String -> Preview.Msg
toDocs body =
  case Json.decodeString Docs.decodePackage body of
    Err _ ->
      Preview.Fail (Just "Could not parse file contents as Elm docs.")

    Ok dict ->
      Preview.LoadDocs dict



-- VIEW


view : Model -> Html Msg
view model =
  Html.map UpdatePreview
    (Header.view model.header (Preview.view model.preview))

