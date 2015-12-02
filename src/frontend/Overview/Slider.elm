module Overview.Slider where

import Effects as Fx
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing ((:=))

import Overview.Constants as Constants



-- MODEL


type alias Model =
    { fraction : Float
    , dragState : Maybe DragInfo
    }


type alias DragInfo =
    { start : Int
    , current : Int
    }


init : Model
init =
  Model 0.3 Nothing



-- UPDATE


type Action
    = DragStart Int
    | DragAt Int
    | DragEnd


update : Action -> Model -> ( Model, Fx.Effects Action )
update action model =
  flip (,) Fx.none <|
  case action of
    DragStart x ->
      { model |
          dragState = Just (DragInfo x x)
      }

    DragAt x ->
      case model.dragState of
        Nothing ->
          model

        Just dragInfo ->
          { model |
              dragState = Just { dragInfo | current = x }
          }

    DragEnd ->
      Model (currentFraction model) Nothing


currentFraction : Model -> Float
currentFraction {fraction, dragState} =
  let
    offset =
      case dragState of
        Nothing ->
          0

        Just {start,current} ->
          Constants.toFraction (current - start)
  in
    clamp 0 1 (fraction + offset)



-- VIEW


(=>) = (,)


view : Signal.Address Action -> String -> String -> Float -> Html
view address color label fraction =
  button
    [ class "slider-handle"
    , on "mousedown" ("pageX" := Decode.int) (Signal.message address << DragStart)
    , style
        [ "left" => (toString (Constants.toX fraction) ++ "px")
        ]
    ]
    [ span [ style ["color" => color] ] [ text "▲"]
    , br [] []
    , text label
    ]
