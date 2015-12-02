module Page.PackageOverviewNew where

import Dict
import Effects as Fx
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Mouse
import StartApp
import Task

import Docs.Version as Vsn
import Overview.History as History
import Page.Context as Ctx
import Utils.ProximityTree as Prox



-- WIRES


port context : Ctx.OverviewContext


app =
  StartApp.start
    { init = init
    , view = view
    , update = update
    , inputs = [ drags, dragEnds ]
    }


main =
  app.html


port worker : Signal (Task.Task Fx.Never ())
port worker =
  app.tasks


drags : Signal Action
drags =
    Signal.map DragTo Mouse.x


dragEnds : Signal Action
dragEnds =
    Signal.filterMap (maybeDrop DragEnd) DragEnd Mouse.isDown


maybeDrop : a -> Bool -> Maybe a
maybeDrop value drop =
    if drop then
        Nothing
    else
        Just value



-- MODEL


type alias Model =
    { dragging : Maybe DragInfo
    , sliders : Dict.Dict SliderId Int
    }


type alias SliderId =
    Int


type alias DragInfo =
    { sliderId : SliderId
    , offset : Int
    }


init : ( Model, Fx.Effects Action )
init =
  ( Model Nothing (Dict.fromList [ ( 1, 0 ), ( 2, 100 ) ])
  , Fx.none
  )


dummyProxList =
  Prox.fromList (toFloat << .date) History.dummy



-- UPDATE


type Action
    = NoOp
    | DragStart DragInfo
    | DragTo Int
    | DragEnd


update action model =
  case action of
    NoOp ->
      model => Fx.none

    DragStart dragInfo ->
      { model | dragging = Just dragInfo } => Fx.none

    DragTo position ->
      case model.dragging of
          Nothing ->
              model => Fx.none

          Just { sliderId, offset } ->
              let
                  newSliders =
                      Dict.insert sliderId (position - offset) model.sliders
              in
                  { model | sliders = newSliders } => Fx.none

    DragEnd ->
      { model | dragging = Nothing } => Fx.none



-- VIEW


(=>) = (,)


view address model =
  div
    [ class "center"
    , style [ "padding-top" => "150px" ]
    ]
    [ History.view 920 dummyProxList
    , viewSliders (DragStart >> Signal.message address) model
    , div [ class "diff" ]
        [ h1 [] (diffHeaderText (2,1,1) (3,0,0))
        ]
    ]


diffHeaderText lower higher =
  [ text "Changelog from "
  , span [ style [ "border-bottom" => "4px solid #7FD13B" ] ] [ text (Vsn.vsnToString lower) ]
  , text " to "
  , span [ style [ "border-bottom" => "4px solid #60B5CC" ] ] [ text (Vsn.vsnToString higher) ]
  ]


viewSliders : (DragInfo -> Signal.Message) -> Model -> Html
viewSliders handleDragStart model =
  let
    sliders =
      model.sliders
        |> Dict.toList
        |> List.map (\( id, pos ) -> viewSlider (DragInfo id >> handleDragStart) pos)
  in
    div
      [ class "slider-container" ]
      (text "Move these sliders!" :: sliders)


viewSlider : (Int -> Signal.Message) -> Int -> Html
viewSlider setDragOffset position =
  button
    [ class "slider-handle"
    , on
        "mousedown"
        (Decode.at [ "target", "parentNode", "offsetLeft" ] Decode.int)
        setDragOffset
    , style
        [ "left" => (toString position ++ "px")
        ]
    ]
    [ span [ style ["color" => "#60B5CC"] ] [ text "▲"]
    , br [] []
    , text "2.0.3"
    ]

