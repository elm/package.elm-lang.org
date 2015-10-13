module Page.PackageList where

import Effects as Fx exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import StartApp
import Task

import Header
import Docs.Module as MDocs


-- WIRES

port title : String
port title =
    "Elm Packages"


app =
  StartApp.start
    { init = init
    , view = view
    , update = update
    , inputs = []
    }


main =
  app.html


port worker : Signal (Task.Task Fx.Never ())
port worker =
  app.tasks



-- MODEL


type alias Model =
    { header : Header.Model
    , moduleDocs : MDocs.Model
    }



-- INIT


init : (Model, Effects Action)
init =
  let
    ( moduleDocs, fx ) =
        MDocs.init (MDocs.Context "elm-lang" "core" "3.0.0" "Graphics.Element")
  in
    ( Model Header.dummy moduleDocs
    , Fx.map UpdateDocs fx
    )



-- UPDATE


type Action
    = UpdateDocs MDocs.Action


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    UpdateDocs act ->
        let
          (newDocs, fx) =
            MDocs.update act model.moduleDocs
        in
          ( Model model.header newDocs
          , Fx.map UpdateDocs fx
          )



-- VIEW


view : Signal.Address Action -> Model -> Html
view addr model =
  div []
    [ Header.view addr model.header
    , MDocs.view (Signal.forwardTo addr UpdateDocs) model.moduleDocs
    ]





