module Page.Home where

import Effects as Fx exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import Markdown
import StartApp
import Task

import Component.Header as Header
import Route



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
    }



-- INIT


init : (Model, Effects a)
init =
  let
    (header, headerFx) =
      Header.init Route.Home
  in
    ( Model header
    , headerFx
    )



-- UPDATE


update : a -> Model -> (Model, Effects a)
update action model =
    ( model
    , Fx.none
    )



-- VIEW


(=>) = (,)


view : Signal.Address action -> Model -> Html
view addr model =
  div []
    [ Header.view addr model.header
    ]


