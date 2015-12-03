module Overview.Slider where

import Easing exposing (ease, easeOutExpo, float)
import Effects as Fx
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing ((:=))
import Time exposing (Time)

import Overview.Constants as Constants
import Native.Drag
import Utils.ProximityTree as Prox



-- MODEL


type alias Model =
    { fraction : Float
    , drag : Maybe Drag
    , animation : Animation
    }


type alias Drag =
    { start : Int
    , current : Int
    }


type Animation
    = None
    | Start Float
    | Running Info


type alias Info =
    { target : Float
    , prevClockTime : Time
    , elapsedTime : Time
    }


animationDuration : Time
animationDuration =
  1000


init : Float -> Model
init fraction =
  Model fraction Nothing None



-- UPDATE


type Action
    = DragStart (Signal.Address Action) Int
    | DragAt Int
    | DragEnd
    | Tick Time


update : Prox.ProximityTree a -> Action -> Model -> ( Model, Fx.Effects Action )
update proxTree action model =
  case action of
    DragStart address x ->
      let
        newModel =
          Model
            (currentFraction model)
            (Just (Drag x x))
            None
      in
        (newModel, trackDrags address)

    DragAt x ->
      let
        newModel =
          case model.drag of
            Nothing ->
              model

            Just dragInfo ->
              { model |
                  drag = Just { dragInfo | current = x }
              }
      in
        (newModel, Fx.none)

    DragEnd ->
      let
        fraction =
          currentFraction model

        target =
          fst (Prox.nearest fraction proxTree)
      in
        ( Model fraction Nothing (Start target)
        , Fx.tick Tick
        )

    Tick clockTime ->
      case model.animation of
        None ->
          (model, Fx.none)

        Start target ->
          ( { model | animation = Running (Info target clockTime 0) }
          , Fx.tick Tick
          )

        Running {target, elapsedTime, prevClockTime} ->
          let
            newElapsedTime =
              elapsedTime + (clockTime - prevClockTime)
          in
            if newElapsedTime > animationDuration then
              ( Model target Nothing None
              , Fx.none
              )

            else
              ( { model | animation = Running (Info target clockTime newElapsedTime) }
              , Fx.tick Tick
              )


currentFraction : Model -> Float
currentFraction {fraction, drag, animation} =
  let
    dragOffset =
      case drag of
        Nothing ->
          0

        Just {start,current} ->
          Constants.toFraction (current - start)

    animationOffset =
      case animation of
        None ->
          0

        Start _ ->
          0

        Running { target, elapsedTime } ->
          ease easeOutExpo float 0 (target - fraction) animationDuration elapsedTime
  in
    clamp 0 1 (fraction + dragOffset + animationOffset)



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
