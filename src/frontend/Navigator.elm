module Navigator where

import Effects as Fx exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)

import Header


-- MODEL

type alias Model =
    { readme : Maybe String
    , modules : List String
    , context : Header.Model
    }


dummy : Model
dummy =
  Model "hello, how are you?" ["Task", "Result"] Header.dummy


-- UPDATE

update : a -> Model -> (Model, Effects a)
update action model =
  (model, Fx.none)


-- VIEW

(=>) = (,)


view : Signal.Address a -> Model -> Html
view _ model =
  div []
    [ div
        [ style [ "padding" => "10px 0", "background-color" => "#401155", "color" => "white" ] ]
        [ headerLinks model ]
    , p [ style [ "text-align" => "center", "background-color" => "#cccccc" ] ]
        (versionWarning model)
    ]


