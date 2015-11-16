module Page.PackageOverview where

import Effects as Fx exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import StartApp
import Task

import Component.Header as Header
import Component.PackageOverview as Overview
import Page.Context as Ctx
import Route



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
    { header : Header.Model
    , overview : Overview.Model
    }



-- INIT


init : (Model, Effects Action)
init =
  let
    (header, headerFx) =
      Header.init (Route.fromOverviewContext context)

    (overview, moduleFx) =
      Overview.init context
  in
    ( Model header overview
    , Fx.batch
        [ headerFx
        , Fx.map UpdateOverview moduleFx
        ]
    )



-- UPDATE


type Action
    = UpdateOverview Overview.Action


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    UpdateOverview act ->
        let
          (newDocs, fx) =
            Overview.update act model.overview
        in
          ( { model | overview = newDocs }
          , Fx.map UpdateOverview fx
          )



-- VIEW


view : Signal.Address Action -> Model -> Html
view addr model =
  Header.view addr model.header
    [ Overview.view (Signal.forwardTo addr UpdateOverview) model.overview
    ]


