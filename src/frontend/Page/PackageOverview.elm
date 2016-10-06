module Page.PackageOverview exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Task

import Component.Header as Header
import Component.PackageOverview as Overview
import Page.Context as Ctx
import Route



-- WIRES


main =
  Html.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }



-- MODEL


type alias Model =
    { header : Header.Model
    , overview : Overview.Model
    }



-- INIT


init : Ctx.OverviewContext -> (Model, Cmd Msg)
init context =
  let
    (header, headerCmd) =
      Header.init (Route.fromOverviewContext context)

    (overview, moduleCmd) =
      Overview.init context
  in
    Model header overview
      ! [ headerCmd
        , Cmd.map UpdateOverview moduleCmd
        ]



-- UPDATE


type Msg
    = UpdateOverview Overview.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateOverview act ->
        let
          (newDocs, fx) =
            Overview.update act model.overview
        in
          ( { model | overview = newDocs }
          , Cmd.map UpdateOverview fx
          )



-- VIEW


view : Model -> Html Msg
view model =
  Header.view model.header
    [ Html.map UpdateOverview (Overview.view model.overview)
    ]


