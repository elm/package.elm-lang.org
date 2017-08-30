module Page.Docs exposing
  ( Model
  , Metadata
  , Content(..)
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
  { metadata : Metadata
  , content : Content
  , query : String
  }


type alias Metadata =
  { user : String
  , project : String
  , version : Route.Version
  , info : Route.VersionInfo
  , latest : Maybe Version.Version
  }


type Content
  = Module String Docs.Module (List Docs.Module)
  | Readme String (List Docs.Module)



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
toTitle model =
  metadataToTitle model.metadata


metadataToTitle : Metadata -> String
metadataToTitle { project, version, info } =
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


view : Model -> App.Body Msg
view { metadata, content, query } =
  App.body [] <|
    case content of
      Readme readme allDocs ->
        [ div [ class "block-list" ] [ Markdown.block readme ]
        , lazy3 viewSidebar metadata allDocs query
        ]

      Module name docs allDocs ->
        [ lazy4 viewModule metadata name docs allDocs
        , lazy3 viewSidebar metadata allDocs query
        ]


viewModule : Metadata -> String -> Docs.Module -> List Docs.Module -> Html Msg
viewModule { user, project, version } name docs allDocs =
  let
    header =
      h1 [class "block-list-title"] [ text name ]

    info =
      Block.makeInfo user project version name allDocs

    blocks =
      List.map (Block.view info) (Docs.toBlocks docs)
  in
  Html.map Push <|
    div [ class "block-list" ] (header :: blocks)



-- VIEW SIDEBAR


viewSidebar : Metadata -> List Docs.Module -> String -> Html Msg
viewSidebar metadata allDocs query =
  div
    [ class "pkg-nav"
    ]
    [ lazy viewReadmeLink metadata
    , br [] []
    , lazy viewBrowseSourceLink metadata
    , h2 [] [ text "Module Docs" ]
    , input
        [ placeholder "Search"
        , value query
        , onInput Search
        ]
        []
    , viewSidebarModules metadata allDocs query
    ]


viewSidebarModules : Metadata -> List Docs.Module -> String -> Html Msg
viewSidebarModules metadata docs query =
  if String.isEmpty query then
    let
      viewEntry docs =
        li [] [ viewModuleLink metadata docs.name ]
    in
    ul [] (List.map viewEntry docs)

  else
    ul [] <|
      List.filterMap (viewSearchItem metadata (String.toLower query)) docs


viewSearchItem : Metadata -> String -> Docs.Module -> Maybe (Html Msg)
viewSearchItem metadata query docs =
  let
    toItem ownerName valueName =
      viewValueItem metadata docs.name ownerName valueName

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
          [ viewModuleLink metadata docs.name
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



-- VIEW "README" LINK


viewReadmeLink : Metadata -> Html Msg
viewReadmeLink { user, project, version, info } =
  navLink
    "README"
    (Route.Version user project version Route.Readme)
    (info == Route.Readme)



-- VIEW "BROWSE SOURCE" LINK


viewBrowseSourceLink : Metadata -> Html msg
viewBrowseSourceLink { user, project, version, latest } =
  case version of
    Route.Exactly vsn ->
      viewBrowseSourceLinkHelp user project vsn

    Route.Latest ->
      case latest of
        Just vsn ->
          viewBrowseSourceLinkHelp user project vsn

        Nothing ->
          text "Browse Source"


viewBrowseSourceLinkHelp : String -> String -> Version.Version -> Html msg
viewBrowseSourceLinkHelp user project version =
  let
    url =
      Url.crossOrigin
        "https://github.com"
        [ user, project, "tree", Version.toString version ]
        []
  in
  a [ class "pkg-nav-module", href url ] [ text "Browse Source" ]



-- VIEW "MODULE" LINK


viewModuleLink : Metadata -> String -> Html Msg
viewModuleLink { user, project, version, info } name =
  let
    route =
      Route.Version user project version (Route.Module name Nothing)
  in
  navLink name route <|
    case info of
      Route.Readme ->
        False

      Route.Module selectedName _ ->
        selectedName == name


viewValueItem : Metadata -> String -> String -> String -> Html Msg
viewValueItem { user, project, version } moduleName ownerName valueName =
  let
    route =
      Route.Version user project version (Route.Module moduleName (Just ownerName))
  in
  li [ class "pkg-nav-value" ] [ link valueName route ]



-- LINK HELPERS


link : String -> Route.Route -> Html Msg
link name route =
  navLink name route False


navLink : String -> Route.Route -> Bool -> Html Msg
navLink name route isBold =
  let
    attributes =
      if isBold then
        [ class "pkg-nav-module"
        , style "font-weight" "bold"
        , style "text-decoration" "underline"
        ]
      else
        [ class "pkg-nav-module"
        ]
  in
  App.link Push route attributes [ text name ]
