module Page.Docs exposing
  ( Model
  , Focus(..)
  , init
  , Msg
  , update
  , view
  , toTitle
  )


import Browser.Dom as Dom
import Elm.Docs as Docs
import Elm.Version as V
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (..)
import Http
import Href
import Page.Docs.Block as Block
import Page.Problem as Problem
import Release
import Session
import Skeleton
import Task
import Url.Builder as Url
import Utils.Markdown as Markdown
import Utils.OneOrMore exposing (OneOrMore)



-- MODEL


type alias Model =
  { session : Session.Data
  , author : String
  , project : String
  , version : Maybe V.Version
  , focus : Focus
  , query : String
  , latest : Status V.Version
  , readme : Status String
  , docs : Status (List Docs.Module)
  }


type Focus
  = Readme
  | Module String (Maybe String)


type Status a
  = Failure
  | Loading
  | Success a


type DocsError
  = NotFound
  | FoundButMissingModule



-- INIT


init : Session.Data -> String -> String -> Maybe V.Version -> Focus -> ( Model, Cmd Msg )
init session author project version focus =
  case Session.getReleases session author project of
    Just releases ->
      let
        latest = Release.getLatest releases
      in
      getInfo latest <|
        Model session author project version focus "" (Success latest) Loading Loading

    Nothing ->
      ( Model session author project version focus "" Loading Loading Loading
      , Http.send GotReleases (Session.fetchReleases author project)
      )


getInfo : V.Version -> Model -> ( Model, Cmd Msg )
getInfo latest model =
  let
    author = model.author
    project = model.project
    version = Maybe.withDefault latest model.version
    maybeInfo =
      Maybe.map2 Tuple.pair
        (Session.getReadme model.session author project version)
        (Session.getDocs model.session author project version)
  in
  case maybeInfo of
    Nothing ->
      ( model
      , Cmd.batch
          [ Http.send (GotReadme version) (Session.fetchReadme author project version)
          , Http.send (GotDocs version) (Session.fetchDocs author project version)
          ]
      )

    Just (readme, docs) ->
      ( { model
            | readme = Success readme
            , docs = Success docs
        }
      , scrollIfNeeded model.focus
      )


scrollIfNeeded : Focus -> Cmd Msg
scrollIfNeeded focus =
  case focus of
    Module _ (Just tag) ->
      Task.attempt ScrollAttempted (
        Dom.getElement tag
          |> Task.andThen (\info -> Dom.setViewport 0 info.element.y)
      )

    _ ->
      Cmd.none



-- UPDATE


type Msg
  = QueryChanged String
  | ScrollAttempted (Result Dom.Error ())
  | GotReleases (Result Http.Error (OneOrMore Release.Release))
  | GotReadme V.Version (Result Http.Error String)
  | GotDocs V.Version (Result Http.Error (List Docs.Module))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    QueryChanged query ->
      ( { model | query = query }
      , Cmd.none
      )

    ScrollAttempted _ ->
      ( model
      , Cmd.none
      )

    GotReleases result ->
      case result of
        Err _ ->
          ( { model
                | latest = Failure
                , readme = Failure
                , docs = Failure
            }
          , Cmd.none
          )

        Ok releases ->
          let
            latest = Release.getLatest releases
          in
          getInfo latest
            { model
                | latest = Success latest
                , session = Session.addReleases model.author model.project releases model.session
            }

    GotReadme version result ->
      case result of
        Err _ ->
          ( { model | readme = Failure }
          , Cmd.none
          )

        Ok readme ->
          ( { model
                | readme = Success readme
                , session = Session.addReadme model.author model.project version readme model.session
            }
          , Cmd.none
          )

    GotDocs version result ->
      case result of
        Err _ ->
          ( { model | docs = Failure }
          , Cmd.none
          )

        Ok docs ->
          ( { model
                | docs = Success docs
                , session = Session.addDocs model.author model.project version docs model.session
            }
          , scrollIfNeeded model.focus
          )



-- VIEW


view : Model -> Skeleton.Details Msg
view model =
  { title = toTitle model
  , header = toHeader model
  , warning = toWarning model
  , attrs = []
  , kids =
      [ viewContent model
      , viewSidebar model
      ]
  }



-- TITLE


toTitle : Model -> String
toTitle model =
  case model.focus of
    Readme ->
      toGenericTitle model

    Module name _ ->
      name ++ " - " ++ toGenericTitle model


toGenericTitle : Model -> String
toGenericTitle model =
  case getVersion model of
    Just version ->
      model.project ++ " " ++ V.toString version

    Nothing ->
      model.project


getVersion : Model -> Maybe V.Version
getVersion model =
  case model.version of
    Just version ->
      model.version

    Nothing ->
      case model.latest of
        Success version -> Just version
        Loading -> Nothing
        Failure -> Nothing



-- TO HEADER


toHeader : Model -> List Skeleton.Segment
toHeader model =
  [ Skeleton.authorSegment model.author
  , Skeleton.projectSegment model.author model.project
  , Skeleton.versionSegment model.author model.project (getVersion model)
  ]
  ++
    case model.focus of
      Readme ->
        []

      Module name _ ->
        [ Skeleton.moduleSegment model.author model.project model.version name
        ]



-- WARNING


toWarning : Model -> Skeleton.Warning
toWarning model =
  case model.version of
    Nothing ->
      Skeleton.NoProblems

    Just version ->
      case model.latest of
        Success latest ->
          if version == latest then
            Skeleton.NoProblems
          else
            Skeleton.NewerVersion (toNewerUrl model) latest

        Loading ->
          Skeleton.NoProblems

        Failure ->
          Skeleton.NoProblems


toNewerUrl : Model -> String
toNewerUrl model =
  case model.focus of
    Readme ->
      Href.toVersion model.author model.project Nothing

    Module name tag ->
      Href.toModule model.author model.project Nothing name tag



-- VIEW CONTENT


viewContent : Model -> Html msg
viewContent model =
  case model.focus of
    Readme ->
      lazy viewReadme model.readme

    Module name tag ->
      lazy5 viewModule model.author model.project model.version name model.docs



-- VIEW README


viewReadme : Status String -> Html msg
viewReadme status =
  case status of
    Success readme ->
      div [ class "block-list" ] [ Markdown.block readme ]

    Loading ->
      div [ class "block-list" ] [ text "" ] -- TODO

    Failure ->
      div
        (class "block-list" :: Problem.styles)
        (Problem.offline "README.md")



-- VIEW MODULE


viewModule : String -> String -> Maybe V.Version -> String -> Status (List Docs.Module) -> Html msg
viewModule author project version name status =
  case status of
    Success allDocs ->
      case findModule name allDocs of
        Just docs ->
          let
            header = h1 [class "block-list-title"] [ text name ]
            info = Block.makeInfo author project version name allDocs
            blocks = List.map (Block.view info) (Docs.toBlocks docs)
          in
          div [ class "block-list" ] (header :: blocks)

        Nothing ->
          div
            (class "block-list" :: Problem.styles)
            (Problem.missingModule author project version name)

    Loading ->
      div [ class "block-list" ]
        [ h1 [class "block-list-title"] [ text name ] -- TODO better loading
        ]

    Failure ->
      div
        (class "block-list" :: Problem.styles)
        (Problem.offline "docs.json")


findModule : String -> List Docs.Module -> Maybe Docs.Module
findModule name docsList =
  case docsList of
    [] ->
      Nothing

    docs :: otherDocs ->
      if docs.name == name then
        Just docs
      else
        findModule name otherDocs



-- VIEW SIDEBAR


viewSidebar : Model -> Html Msg
viewSidebar model =
  div
    [ class "pkg-nav"
    ]
    [ lazy4 viewReadmeLink model.author model.project model.version model.focus
    , br [] []
    , lazy4 viewBrowseSourceLink model.author model.project model.version model.latest
    , h2 [] [ text "Install" ]
    , lazy2 viewInstallInstruction model.author model.project
    , h2 [] [ text "Module Docs" ]
    , input
        [ placeholder "Search"
        , value model.query
        , onInput QueryChanged
        ]
        []
    , viewSidebarModules model
    ]


viewSidebarModules : Model -> Html msg
viewSidebarModules model =
  case model.docs of
    Failure ->
      text "" -- TODO

    Loading ->
      text "" -- TODO

    Success modules ->
      if String.isEmpty model.query then
        let
          viewEntry docs =
            li [] [ viewModuleLink model docs.name ]
        in
        ul [] (List.map viewEntry modules)

      else
        let
          query =
            String.toLower model.query
        in
        ul [] (List.filterMap (viewSearchItem model query) modules)


viewSearchItem : Model -> String -> Docs.Module -> Maybe (Html msg)
viewSearchItem model query docs =
  let
    toItem ownerName valueName =
      viewValueItem model docs.name ownerName valueName

    matches =
      List.filterMap (isMatch query toItem) docs.binops
      ++ List.concatMap (isUnionMatch query toItem) docs.unions
      ++ List.filterMap (isMatch query toItem) docs.aliases
      ++ List.filterMap (isMatch query toItem) docs.values
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


isMatch : String -> (String -> String -> b) -> { r | name : String } -> Maybe b
isMatch query toResult {name} =
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



-- VIEW "README" LINK


viewReadmeLink : String -> String -> Maybe V.Version -> Focus -> Html msg
viewReadmeLink author project version focus =
  navLink "README" (Href.toVersion author project version) <|
    case focus of
      Readme -> True
      Module _ _ -> False



-- VIEW "BROWSE SOURCE" LINK


viewBrowseSourceLink : String -> String -> Maybe V.Version -> Status V.Version -> Html msg
viewBrowseSourceLink author project maybeVersion latest =
  case maybeVersion of
    Just version ->
      viewBrowseSourceLinkHelp author project version

    Nothing ->
      case latest of
        Success version ->
          viewBrowseSourceLinkHelp author project version

        Loading ->
          text "Browse Source"

        Failure ->
          text "Browse Source"


viewBrowseSourceLinkHelp : String -> String -> V.Version -> Html msg
viewBrowseSourceLinkHelp author project version =
  let
    url =
      Url.crossOrigin
        "https://github.com"
        [ author, project, "tree", V.toString version ]
        []
  in
  a [ class "pkg-nav-module", href url ] [ text "Browse Source" ]



-- VIEW INSTALL INSTRUCTION


viewInstallInstruction : String -> String -> Html msg
viewInstallInstruction author project =
  code [ class "pkg-nav-install" ]
    [ span [] [ text "> "]
    , text ("elm install " ++ author ++ "/" ++ project)
    ]


-- VIEW "MODULE" LINK


viewModuleLink : Model -> String -> Html msg
viewModuleLink model name =
  let
    url =
      Href.toModule model.author model.project model.version name Nothing
  in
  navLink name url <|
    case model.focus of
      Readme ->
        False

      Module selectedName _ ->
        selectedName == name


viewValueItem : Model -> String -> String -> String -> Html msg
viewValueItem { author, project, version } moduleName ownerName valueName =
  let
    url =
      Href.toModule author project version moduleName (Just ownerName)
  in
  li [ class "pkg-nav-value" ] [ navLink valueName url False ]



-- LINK HELPERS


navLink : String -> String -> Bool -> Html msg
navLink name url isBold =
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
  a (href url :: attributes) [ text name ]
