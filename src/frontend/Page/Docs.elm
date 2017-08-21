module Page.Docs exposing
  ( Model
  , Msg
  , update
  , view
  , toTitle
  )


import Browser.History as History
import Elm.Docs as Docs
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (..)
import Page.Docs.Block as Block
import Route
import Url
import Utils.App as App
import Utils.Markdown as Markdown
import Version



-- MODEL


type alias Model =
  { user : String
  , project : String
  , version : Route.Version
  , info : Route.VersionInfo
  , readme : Maybe String
  , docs : Maybe (List Docs.Module)
  , query : String
  }



-- UPDATE


type Msg
  = Push Route.Route
  | Search String


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
  case msg of
    Push route ->
      ( model
      , History.push (Route.toUrl route)
      )

    Search query ->
      ( { model | query = query }
      , Cmd.none
      )



-- TO TITLE


toTitle : Model -> String
toTitle { project, version, info } =
  let
    genericTitle =
      project ++ " " ++ Route.vsnToString version
  in
  case info of
    Route.Readme ->
      genericTitle

    Route.Module name _ ->
      name ++ " - " ++ genericTitle



-- VIEW


view : (Msg -> msg) -> Model -> List (Html msg)
view toMsg ({ user, project, version, docs } as model) =
  let
    mainContent =
      case model.info of
        Route.Readme ->
          lazy viewReadme model.readme

        Route.Module name _ ->
          lazy5 viewModule user project version name docs
  in
    [ Html.map toMsg <| mainContent
    , Html.map toMsg <| lazy viewSidebar model
    ]


viewReadme : Maybe String -> Html msg
viewReadme loadingReadme =
  let
    content =
      case loadingReadme of
        Nothing ->
          text "Loading..."

        Just readme ->
          Markdown.block readme
  in
    div [ class "block-list" ] [ content ]


viewModule : String -> String -> Route.Version -> String -> Maybe (List Docs.Module) -> Html Msg
viewModule user project version name loadingDocs =
  Html.map Push <|
  div [ class "block-list" ] <|
    h1 [class "block-list-title"] [ text name ]
    ::
    case loadingDocs of
      Nothing ->
        [ text "Loading..."
        ]

      Just docsList ->
        case findDocs name docsList of
          Nothing ->
            [ text "Something went wrong."
            ]

          Just docs ->
            let
              info =
                Block.makeInfo user project version name docsList
            in
            List.map (Block.view info) (Docs.toBlocks docs)


findDocs : String -> List Docs.Module -> Maybe Docs.Module
findDocs name docsList =
  case docsList of
    [] ->
      Nothing

    docs :: rest ->
      if docs.name == name then
        Just docs
      else
        findDocs name rest



-- VIEW SIDEBAR


viewSidebar : Model -> Html Msg
viewSidebar ({ user, project, version, info, query } as model) =
  div
    [ class "pkg-nav"
    ]
    [ lazy4 viewReadmeLink user project version info
    , br [] []
    , lazy3 viewBrowseSourceLink user project version
    , h2 [] [ text "Module Docs" ]
    , input
        [ placeholder "Search"
        , value query
        , onInput Search
        ]
        []
    , viewSidebarModules model
    ]


viewSidebarModules : Model -> Html Msg
viewSidebarModules model =
  case model.docs of
    Nothing ->
      text "Loading..."

    Just docs ->
      if String.isEmpty model.query then
        let
          viewEntry docs =
            li [] [ viewModuleLink model docs.name ]
        in
          ul [] (List.map viewEntry docs)

      else
        ul [] <|
          List.filterMap (viewSearchItem model (String.toLower model.query)) docs


viewSearchItem : Model -> String -> Docs.Module -> Maybe (Html Msg)
viewSearchItem model query docs =
  let
    toItem ownerName valueName =
      viewValueItem model docs.name ownerName valueName

    matches =
      List.concatMap (isUnionMatch query toItem) docs.unions
      ++ List.filterMap (isMatch query identity toItem) docs.aliases
      ++ List.filterMap (isMatch query getName toItem) docs.values
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


isMatch : String -> (a -> String) -> (String -> String -> b) -> { r | name : a } -> Maybe b
isMatch query toName toResult entry =
  let
    name =
      toName entry.name
  in
  if String.contains query (String.toLower name) then
    Just (toResult name name)
  else
    Nothing


isUnionMatch : String -> (String -> String -> a) -> Docs.Union -> List a
isUnionMatch query toResult {name,tags} =
  let
    tagMatches =
      List.filterMap (isTagMatch query toResult name) tags
  in
    if String.contains query (String.toLower name) then
      toResult name name :: tagMatches
    else
      tagMatches


isTagMatch : String -> (String -> String -> a) -> String -> (String, details) -> Maybe a
isTagMatch query toResult tipeName (tagName, _) =
  if String.contains query (String.toLower tagName) then
    Just (toResult tipeName tagName)
  else
    Nothing


getName : Docs.Name -> String
getName valueName =
  case valueName of
    Docs.Name name ->
      name

    Docs.Op name _ _ ->
      name


-- VIEW SIDEBAR LINKS


viewReadmeLink : String -> String -> Route.Version -> Route.VersionInfo -> Html Msg
viewReadmeLink user project version info =
  let
    route =
      Route.Version user project version Route.Readme
  in
    case info of
      Route.Readme ->
        boldNavLink "README" route

      Route.Module _ _ ->
        navLink "README" route


viewBrowseSourceLink : String -> String -> Route.Version -> Html msg
viewBrowseSourceLink user project version =
  case version of
    Route.Latest ->
      text "Browse Source"

    Route.Exactly vsn ->
      a [ class "pkg-nav-module"
        , href <|
            Url.crossOrigin "https://github.com" [ user, project, "tree", Version.toString vsn ] []
        ]
        [ text "Browse Source" ]


viewModuleLink : Model -> String -> Html Msg
viewModuleLink { user, project, version, info } name =
  let
    route =
      Route.Version user project version (Route.Module name Nothing)
  in
    case info of
      Route.Readme ->
        navLink name route

      Route.Module selectedName _ ->
        if selectedName == name then
          boldNavLink name route
        else
          navLink name route


viewValueItem : Model -> String -> String -> String -> Html Msg
viewValueItem { user, project, version } moduleName ownerName valueName =
  let
    route =
      Route.Version user project version (Route.Module moduleName (Just ownerName))
  in
    li [ class "pkg-nav-value" ] [ navLink valueName route ]


navLink : String -> Route.Route -> Html Msg
navLink name route =
  App.link Push route [ class "pkg-nav-module" ] [ text name ]


boldNavLink : String -> Route.Route -> Html Msg
boldNavLink name route =
  App.link Push route [ class "pkg-nav-module" ]
    [ span
        [ style "font-weight" "bold"
        , style "text-decoration" "underline"
        ]
        [ text name ]
    ]
