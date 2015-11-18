module Page.Catalog where

import Effects as Fx exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import StartApp
import Task

import Component.Catalog as Catalog
import Component.CatalogSidebar as Sidebar
import Component.Header as Header
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
    , catalog : Catalog.Model
    , sidebar : Sidebar.Model
    }



-- INIT


init : (Model, Effects Action)
init =
  let
    (header, headerFx) =
      Header.init (Route.Packages Nothing)

    (catalog, catalogFx) =
      Catalog.init

    (sidebar, sidebarFx) =
      Sidebar.init
  in
    ( Model header catalog sidebar
    , Fx.batch
        [ headerFx
        , Fx.map UpdateCatalog catalogFx
        , Fx.map UpdateSidebar sidebarFx
        ]
    )



-- UPDATE


type Action
    = UpdateCatalog Catalog.Action
    | UpdateSidebar Sidebar.Action


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    UpdateCatalog act ->
        let
          (newCatalog, fx) =
            Catalog.update act model.catalog
        in
          ( { model | catalog = newCatalog }
          , Fx.map UpdateCatalog fx
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
    [ Catalog.view (Signal.forwardTo addr UpdateCatalog) model.catalog
    , Sidebar.view (Signal.forwardTo addr UpdateSidebar) model.sidebar
    ]


