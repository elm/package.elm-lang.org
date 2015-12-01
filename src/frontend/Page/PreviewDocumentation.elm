module Page.PreviewDocumentation where

import Effects as Fx exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Json
import StartApp
import Task

import Component.Header as Header
import Component.PackagePreview as Preview
import Docs.Package as Docs
import Route



-- WIRES


app =
  StartApp.start
    { init = init
    , view = view
    , update = update
    , inputs = [ docsUploads ]
    }


main =
  app.html


port worker : Signal (Task.Task Fx.Never ())
port worker =
  app.tasks


port uploads : Signal String


docsUploads : Signal Action
docsUploads =
  let
    toDocs body =
      case Json.decodeString Docs.decodePackage body of
        Err _ ->
          Preview.Fail (Just "Could not parse file contents as Elm docs.")

        Ok dict ->
          Preview.LoadDocs dict
  in
    Signal.map (UpdatePreview << toDocs) uploads



-- MODEL


type alias Model =
    { header : Header.Model
    , preview : Preview.Model
    }



-- INIT


init : (Model, Effects Action)
init =
  let
    (header, headerFx) =
      Header.init Route.Help

    (preview, previewFx) =
      Preview.init
  in
    ( Model header preview
    , Fx.batch
        [ headerFx
        , Fx.map UpdatePreview previewFx
        ]
    )



-- UPDATE


type Action
    = UpdatePreview Preview.Action


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    UpdatePreview act ->
        let
          (newPreview, fx) =
            Preview.update act model.preview
        in
          ( { model | preview = newPreview }
          , Fx.map UpdatePreview fx
          )



-- VIEW


view : Signal.Address Action -> Model -> Html
view addr model =
  Header.view addr model.header
    (Preview.view (Signal.forwardTo addr UpdatePreview) model.preview)

