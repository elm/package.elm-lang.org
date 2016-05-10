module Component.Blog exposing (..)

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


init : String -> (Model, Cmd msg)
init blog =
  let
    (header, headerCmd) =
      Header.init Route.Help
  in
    ( Model header blog
    , headerCmd
    )



-- UPDATE


update : msg -> Model -> (Model, Cmd msg)
update msg model =
  (model, Cmd.none)



-- VIEW


view : Model -> Html msg
view model =
  Header.view model.header
    [ div [style [("width", "600px")]] [ Markdown.block model.blog ]
    ]

