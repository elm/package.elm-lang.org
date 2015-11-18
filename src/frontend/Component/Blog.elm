module Component.Blog where

import Effects as Fx exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)

import Component.Header as Header
import Route
import Utils.Markdown as Markdown



-- MODEL


type alias Model =
    { header : Header.Model
    , blog : String
    }



-- INIT


init : String -> (Model, Effects a)
init blog =
  let
    (header, headerFx) =
      Header.init Route.Help
  in
    ( Model header blog
    , headerFx
    )



-- UPDATE


update : a -> Model -> (Model, Effects a)
update action model =
  (model, Fx.none)



-- VIEW


view : Signal.Address a -> Model -> Html
view addr model =
  Header.view addr model.header
    [ div [style [("width", "600px")]] [ Markdown.block model.blog ]
    ]

