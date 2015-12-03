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
    (fraction1, version1) =
      Prox.nearest (Slider.currentFraction slider1) versions

    (fraction2, version2) =
      Prox.nearest (Slider.currentFraction slider2) versions

    viewSlider tag frac vsn =
      Slider.view
        (Signal.forwardTo address tag)
        frac
        (Vsn.vsnToString vsn)
        (if vsn == min version1 version2 then "#7FD13B" else "#60B5CC")
  in
    div
      [ class "center"
      , style [ "padding-top" => "150px" ]
      ]
      [ History.view versions
      , div [ class "slider-container" ]
          [ viewSlider UpdateSlider1 fraction1 version1
          , viewSlider UpdateSlider2 fraction2 version2
          ]
      , div [ class "diff" ]
          [ h1 [] (headerText version1 version2)
          ]
      ]


headerText : Vsn.Version -> Vsn.Version -> List Html
headerText lower higher =
  [ text "Changes between "
  , span [ style [ "border-bottom" => "4px solid #7FD13B" ] ] [ text (Vsn.vsnToString lower) ]
  , text " and "
  , span [ style [ "border-bottom" => "4px solid #60B5CC" ] ] [ text (Vsn.vsnToString higher) ]
  ]

