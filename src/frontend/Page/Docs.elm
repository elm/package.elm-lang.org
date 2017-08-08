module Page.Docs exposing
  ( Model
  , initReadme
  , initModule
  , Msg
  , update
  , view
  )


import Html exposing (..)
import Html.Attributes exposing (..)

import Elm.Docs as Docs
import Page.Docs.Block as Block
import Route
import Utils.App as App
import Utils.Markdown as Markdown



-- MODEL


type alias Model =
  { user : String
  , project : String
  , version : Version.Version
  , display : Display
  , query : String
  }


type Display
    = Readme
    | Module String



-- INIT


initReadme : Session.Data -> String -> String -> Version.Version -> ( Model, Session.Data, Cmd Session.Msg )
initReadme session user project version =
  init session user project version Readme


initModule : Session.Data -> String -> String -> Version.Version -> String -> ( Model, Session.Data, Cmd Session.Msg )
initModule session user project version moduleName =
  init session user project version (Module moduleName)


init : Session.Data -> String -> String -> Version.Version -> Display -> ( Model, Session.Data, Cmd Session.Msg )
init session0 user project version display =
  let
    (session1, cmds1) = Session.loadDocs user project version session0
    (session2, cmds2) = Session.loadReadme user project version session1
    (session3, cmds3) = Session.loadReleases user project session2
  in
    ( Model user project version display
    , session3
    , Cmd.batch [ cmds1, cmds2, cmds3 ]
    )



-- UPDATE


type Msg
  = GoTo Route.Route
  | Search String


update : Msg -> Model -> App.Update Model
update msg model =
  case msg of
    GoTo route ->
      App.GoTo route

    Search query ->
      App.Update { model | query = query }



-- VIEW


view : Session.Data -> Model -> Html msg
view session ({ user, project, version, display, query } as model) =
  let
    mainContent =
      case display of
        Readme ->
          lazy viewReadme (Session.getReadme session user project version)

        Module name ->
          lazy5 viewModule session user project version name
  in
    [ mainContent
    , viewSidebar session model
    ]


viewReadme : Session.Remote String -> Html msg
viewReadme remoteReadme =
  let
    content =
      case remoteReadme of
        Session.Loading ->
          text "Loading..."

        Session.Failed ->
          text ""

        Session.Loaded readme ->
          Markdown.block readme
  in
    div [ class "block-list" ] [ content ]


viewModule : Session.Data -> String -> String -> Version.Version -> String -> Html Msg
viewModule session user project version name =
  div [ class "block-list" ] <|
    case Session.getDocs session user project version of
      Session.Loading ->
        [ text "Loading..."
        ]

      Session.Failed ->
        [ text ""
        ]

      Session.Loaded docsList ->
        case findDocs name docsList of
          Nothing ->
            [ p []
                [ text "Module "
                , code [] [ text name ]
                , text " does not exist."
                ]
            , p []
                [ text "Maybe it existed in a previous release? Go to the "
                , "README"
                , text " to get an overview."
                ]
            ]

          Just docs ->
            let
              info =
                Block.makeInfo user project version name docsList
            in
            h1 [class "block-list-title"] [text name]
            :: List.map (Block.view info) (Docs.toBlocks docs)



-- VIEW SIDEBAR


viewSidebar : Session.Data -> Model -> Html Msg
viewSidebar session ({ user, project, version, page query } as model) =
  div
    [ class "pkg-nav"
    ]
    [ lazy4 viewReadmeLink user project version page
    , br [] []
    , lazy3 viewBrowseSourceLink user project version
    , h2 [] [ text "Module Docs" ]
    , input
        [ placeholder "Search"
        , value query
        , onInput Search
        ]
        []
    , viewSidebarModules session model
    ]


viewSidebarModules : Session.Data -> Model -> Html msg
viewSidebarModules session ({ user, project, version, query } as model) =
  case Session.getDocs session user project version of
    Session.Loading ->
      text "Loading..."

    Session.Failed ->
      text ""

    Session.Loaded docs ->
      if String.isEmpty query then
        let
          viewEntry docs =
            li [] [ viewModuleLink model docs.name ]
        in
          ul [] (List.map viewEntry docs)

      else
        ul [] <|
          List.filterMap (viewSearchItem model (String.toLower query)) docs


viewSearchItem : Model -> String -> Docs.Module -> Maybe (Html Msg)
viewSearchItem model query docs =
  let
    matches =
      List.filterMap isUnionMatch docs.unions
      ++ List.filterMap isMatch docs.aliases
      ++ List.filterMap isMatch docs.values

    isMatch {name} =
      if String.contains query (String.toLower name) then
        Just (viewValueItem model docs.name name name)
      else
        Nothing

    isUnionMatch {name,tags} =
      let
        tipe =
          if String.contains query (String.toLower name) then
            [ viewValueItem model docs.name name name ]
          else
            []
      in
        tipe ++ List.filterMap (isTagMatch name) tags

    isTagMatch name (tag, _) =
      if String.contains query (String.toLower tag) then
        Just (viewValueItem model docs.name name tag)
      else
        Nothing
  in
    if List.isEmpty matches && not (String.contains query docs.name) then
      Nothing

    else
      Just <|
        li
          [ class "pkg-nav-search-chunk"
          ]
          [ viewModuleLink model docs.name
          , ul [] matches
          ]



-- VIEW SIDEBAR LINKS


viewReadmeLink : String -> String -> Version.Version -> Page -> Html Msg
viewReadmeLink user project version page =
  let
    route =
      Route.Version user project (Route.Exactly version)
  in
    case page of
      Readme ->
        boldNavLink "README" route

      Module _ ->
        navLink "README" route


viewBrowseSourceLink : String -> String -> Version.Version -> Html msg
viewBrowseSourceLink user project version =
  a [ class "pkg-nav-module"
    , href ("https://github.com" </> user </> project </> "tree" </> Version.toString version)
    ]
    [ text "Browse source" ]


viewModuleLink : Model -> String -> Html Msg
viewModuleLink { user, project, version, page } name =
  let
    route =
      Route.Module user project (Route.Exactly version) name Nothing
  in
    case page of
      Readme ->
        navLink name route

      Module selectedName ->
        if selectedName == name then
          boldNavLink name route
        else
          navLink name route


viewValueItem : Model -> String -> String -> String -> Html Msg
viewValueItem { user, project, version } moduleName ownerName valueName =
  let
    route =
      Route.Module user project (Route.Exactly version) moduleName (Just ownerName)
  in
    li [ class "pkg-nav-value" ] [ navLink valueName route ]


navLink : String -> Route.Route -> Html Msg
navLink name route =
  App.link GoTo route [ class "pkg-nav-module" ] [ text name ]


boldNavLink : String -> Route.Route -> Html Msg
boldNavLink name route =
  App.link GoTo route [ class "pkg-nav-module" ]
    [ span
        [ style "font-weight" "bold"
        , style "text-decoration" "underline"
        ]
        [ text name ]
    ]
