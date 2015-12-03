module Page.PackageOverview where

import Effects as Fx
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (..)
import Json.Decode as Decode
import StartApp
import Task

import Docs.Version as Vsn
import Overview.History as History
import Overview.Slider as Slider
import Page.Context as Ctx
import Utils.ProximityTree as Prox



-- WIRES


port context : Ctx.OverviewContext


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
    { versions : Prox.ProximityTree Vsn.Version
    , slider1 : Slider.Model
    , slider2 : Slider.Model
    }


init : ( Model, Fx.Effects Action )
init =
  let
    history =
      History.dummy

    proxTree =
      Prox.map .version (Prox.fromList (toFloat << .date) history)

    (penultimate, ultimate) =
      latestInterestingVersions history
  in
    ( Model
        proxTree
        (Slider.init (Prox.lookup penultimate proxTree))
        (Slider.init (Prox.lookup ultimate proxTree))
    , Fx.none
    )


latestInterestingVersions : History.History -> (Vsn.Version, Vsn.Version)
latestInterestingVersions history =
  let
    interestingVersions =
      Vsn.filterInteresting (List.map .version history)
  in
    case List.reverse interestingVersions of
      latest :: sndLatest :: _ ->
        ( sndLatest, latest )

      [latest] ->
        ( Vsn.one, latest )

      [] ->
        Debug.crash "How can there be a published package with no versions?"



-- UPDATE


type Action
    = UpdateSlider1 Slider.Action
    | UpdateSlider2 Slider.Action


update : Action -> Model -> ( Model, Fx.Effects Action )
update action model =
  case action of
    UpdateSlider1 act ->
      let
        (newSlider, fx) =
          Slider.update act model.slider1
      in
        ( { model | slider1 = newSlider }
        , Fx.map UpdateSlider1 fx
        )

    UpdateSlider2 act ->
      let
        (newSlider, fx) =
          Slider.update act model.slider2
      in
        ( { model | slider2 = newSlider }
        , Fx.map UpdateSlider2 fx
        )



-- VIEW


(=>) = (,)


view : Signal.Address Action -> Model -> Html
view address {versions, slider1, slider2} =
  let
    fraction1 =
      Slider.currentFraction slider1

    fraction2 =
      Slider.currentFraction slider2

    (_, version1) =
      Prox.nearest fraction1 versions

    (_, version2) =
      Prox.nearest fraction2 versions

    viewSlider tag frac vsn color =
      Slider.view
        (Signal.forwardTo address tag)
        frac
        (Vsn.vsnToString vsn)
        color
  in
    div
      [ class "center"
      , style [ "padding-top" => "150px" ]
      ]
      [ History.view versions
      , div [ class "slider-container" ]
          [ viewSlider UpdateSlider1 fraction1 version1 "#7FD13B"
          , viewSlider UpdateSlider2 fraction2 version2 "#60B5CC"
          ]
      , div [ class "diff" ]
          [ h1 [] (headerText fraction1 fraction2 version1 version2)
          ]
      ]


headerText : Float -> Float -> Vsn.Version -> Vsn.Version -> List Html
headerText fraction1 fraction2 version1 version2 =
  let
    text1 =
      vsnText "#7FD13B" version1

    text2 =
      vsnText "#60B5CC" version2

    (leftVsn, rightVsn) =
      if fraction1 < fraction2 then
        (text1, text2)

      else
        (text2, text1)
  in
    [ text "Changes between ", leftVsn, text " and ", rightVsn ]


vsnText : String -> Vsn.Version -> Html
vsnText color vsn =
  span
    [ style
        [ "border-bottom" => ("4px solid " ++ color)
        ]
    ]
    [ text (Vsn.vsnToString vsn)
    ]


