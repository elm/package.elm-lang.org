module Page.Catalog exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Task

import Component.Catalog as Catalog
import Component.CatalogSidebar as Sidebar
import Component.Header as Header
import Route



-- WIRES


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }



-- MODEL


type alias Model =
    { header : Header.Model
    , catalog : Catalog.Model
    , sidebar : Sidebar.Model
    }



-- INIT


init : (Model, Cmd Msg)
init =
  let
    (header, headerCmd) =
      Header.init (Route.Packages Nothing)

    (catalog, catalogCmd) =
      Catalog.init

    (sidebar, sidebarCmd) =
      Sidebar.init
  in
    Model header catalog sidebar
      ! [ headerCmd
        , Cmd.map UpdateCatalog catalogCmd
        , Cmd.map UpdateSidebar sidebarCmd
        ]



-- UPDATE


type Msg
    = UpdateCatalog Catalog.Msg
    | UpdateSidebar Sidebar.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateCatalog catMsg ->
        let
          (newCatalog, fx) =
            Catalog.update catMsg model.catalog
        in
          { model | catalog = newCatalog }
            ! [ Cmd.map UpdateCatalog fx ]

    UpdateSidebar sideMsg ->
        let
          (newSidebar, fx) =
            Sidebar.update sideMsg model.sidebar
        in
          { model | sidebar = newSidebar }
            ! [ Cmd.map UpdateSidebar fx ]



-- VIEW


view : Model -> Html Msg
view model =
  Header.view model.header
    [ Html.map UpdateCatalog (Catalog.view model.catalog)
    , Html.map UpdateSidebar (Sidebar.view model.sidebar)
    ]


