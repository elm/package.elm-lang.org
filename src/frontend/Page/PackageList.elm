module Page.PackageList where

import Effects as Fx exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import StartApp

import Header
import Docs.Entry


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


-- MODEL

type alias Model =
    { header : Header.Model
    }


init : (Model, Effects act)
init =
  ( Model Header.dummy
  , Fx.none
  )


-- UPDATE

update : act -> Model -> (Model, Effects act)
update action model =
  (model, Fx.none)


-- VIEW

view : Signal.Address act -> Model -> Html
view addr model =
  Header.view addr model.header

