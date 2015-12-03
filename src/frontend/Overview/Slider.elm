module Overview.Slider where

import Effects as Fx
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing ((:=))

import Overview.Constants as Constants
import Native.Drag



-- MODEL


type alias Model =
    { fraction : Float
    , dragState : Maybe DragInfo
    }


type alias DragInfo =
    { start : Int
    , current : Int
    }


init : Float -> Model
init fraction =
  Model fraction Nothing



-- UPDATE


type Action
    = DragStart (Signal.Address Action) Int
    | DragAt Int
    | DragEnd


update : Action -> Model -> ( Model, Fx.Effects Action )
update action model =
  case action of
    DragStart address x ->
      let
        newModel =
          { model | dragState = Just (DragInfo x x) }
      in
        (newModel, trackDrags address)

    DragAt x ->
      let
        newModel =
          case model.dragState of
            Nothing ->
              model

            Just dragInfo ->
              { model |
                  dragState = Just { dragInfo | current = x }
              }
      in
        (newModel, Fx.none)

    DragEnd ->
      ( Model (currentFraction model) Nothing
      , Fx.none
      )


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



-- EFFECTS


trackDrags : Signal.Address Action -> Fx.Effects Action
trackDrags address =
  Fx.task (Native.Drag.track (Signal.send address << DragAt) DragEnd)



-- VIEW


(=>) = (,)


view : Signal.Address Action -> Float -> String -> String -> Html
view address fraction label color =
  button
    [ class "slider-handle"
    , on "mousedown" ("pageX" := Decode.int) (Signal.message address << DragStart address)
    , style
        [ "left" => (toString (Constants.toX fraction) ++ "px")
        ]
    ]
    [ span [ style ["color" => color] ] [ text "▲"]
    , br [] []
    , span
        [ style
            [ "background-color" => color
            , "padding" => "2px 5px"
            , "border-radius" => "4px"
            ]
        ]
        [ text label ]
    ]
