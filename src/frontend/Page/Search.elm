module Page.Search where

import Effects as Fx exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import StartApp
import Task

import Component.CatalogSidebar as Sidebar
import Component.Header as Header
import Component.Search as Search
import Route



-- WIRES


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
    , search : Search.Model
    , sidebar : Sidebar.Model
    }



-- INIT


init : (Model, Effects Action)
init =
  let
    (header, headerFx) =
      Header.init (Route.Packages Nothing)

    (search, searchFx) =
      Search.init

    (sidebar, sidebarFx) =
      Sidebar.init
  in
    ( Model header search sidebar
    , Fx.batch
        [ headerFx
        , Fx.map UpdateSearch searchFx
        , Fx.map UpdateSidebar sidebarFx
        ]
    )



-- UPDATE


type Action
    = UpdateSearch Search.Action
    | UpdateSidebar Sidebar.Action


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    UpdateSearch act ->
        let
          (newSearch, fx) =
            Search.update act model.search
        in
          ( { model | search = newSearch }
          , Fx.map UpdateSearch fx
          )

    UpdateSidebar act ->
        let
          (newSidebar, fx) =
            Sidebar.update act model.sidebar
        in
          ( { model | sidebar = newSidebar }
          , Fx.map UpdateSidebar fx
          )



-- VIEW


view : Signal.Address Action -> Model -> Html
view addr model =
  Header.view addr model.header
    [ Search.view (Signal.forwardTo addr UpdateSearch) model.search
    , Sidebar.view (Signal.forwardTo addr UpdateSidebar) model.sidebar
    ]


