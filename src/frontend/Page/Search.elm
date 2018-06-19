module Page.Search exposing
  ( Model
  , init
  , Msg
  , update
  , view
  )


import Elm.Version as V
import Html exposing (..)
import Html.Attributes exposing (autofocus, class, href, placeholder, style, value)
import Html.Events exposing (..)
import Html.Lazy exposing (..)
import Html.Keyed as Keyed
import Http
import Href
import Json.Decode as Decode
import Page.Search.Entry as Entry
import Page.Problem as Problem
import Session
import Skeleton
import Url.Builder as Url



-- MODEL


type alias Model =
  { session : Session.Data
  , query : String
  , entries : Entries
  }


type Entries
  = Failure
  | Loading
  | Success (List Entry.Entry)


init : Session.Data -> ( Model, Cmd Msg )
init session =
  case Session.getEntries session of
    Just entries ->
      ( Model session "" (Success entries)
      , Cmd.none
      )

    Nothing ->
      ( Model session "" Loading
      , Http.send GotPackages <|
          Http.get "/search.json" (Decode.list Entry.decoder)
      )



-- UPDATE


type Msg
  = QueryChanged String
  | GotPackages (Result Http.Error (List Entry.Entry))


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
  case msg of
    QueryChanged query ->
      ( { model | query = query }
      , Cmd.none
      )

    GotPackages result ->
      case result of
        Err _ ->
          ( { model | entries = Failure }
          , Cmd.none
          )

        Ok entries ->
          ( { model
                | entries = Success entries
                , session = Session.addEntries entries model.session
            }
          , Cmd.none
          )



-- VIEW


view : Model -> Skeleton.Details Msg
view model =
  { title = "Elm Packages"
  , header = []
  , warning = Skeleton.NoProblems
  , attrs = []
  , kids =
      [ lazy2 viewSearch model.query model.entries
      , viewSidebar
      ]
  }



-- VIEW SEARCH


viewSearch : String -> Entries -> Html Msg
viewSearch query entries =
  div [ class "catalog" ]
    [ input
        [ placeholder "Search"
        , value query
        , onInput QueryChanged
        , autofocus True
        ]
        []
    , case entries of
        Failure ->
          div Problem.styles (Problem.offline "search.json")

        Loading ->
          text "" -- TODO

        Success es ->
          Keyed.node "div" [] (List.map viewEntry (Entry.search query es))
    ]



-- VIEW ENTRY


viewEntry : Entry.Entry -> (String, Html msg)
viewEntry entry =
  ( entry.author ++ "/" ++ entry.project
  , lazy viewEntryHelp entry
  )


viewEntryHelp : Entry.Entry -> Html msg
viewEntryHelp ({ author, project, summary } as entry) =
  div [ class "pkg-summary" ]
    [ div []
        [ h1 []
            [ a [ href (Href.toVersion author project Nothing) ]
                [ span [ class "light" ] [ text (author ++ "/") ]
                , text project
                ]
            ]
        , viewExactVersions entry
        ]
    , p [ class "pkg-summary-desc" ] [ text summary ]
    ]


viewExactVersions : Entry.Entry -> Html msg
viewExactVersions entry =
  let
    exactVersion v =
      a [ href (Href.toVersion entry.author entry.project (Just v))
        ]
        [ text (V.toString v)
        ]

    allVersions =
      List.intersperse (text " … ") (List.map exactVersion entry.versions)
      ++
      [ text " — "
      , a [ href (Href.toProject entry.author entry.project) ] [ text "Overview" ]
      ]
  in
  span [ class "pkg-summary-hints" ] <|
    case Maybe.map V.toTuple (List.head entry.versions) of
      Just (1,0,0) ->
        allVersions

      _ ->
        text "… " :: allVersions



-- VIEW SIDEBAR


viewSidebar : Html Msg
viewSidebar =
  div [ class "catalog-sidebar" ]
    [ h2 [] [ text "Popular Packages" ]
    , ul [] <|
        List.map viewPopularPackage [ "core", "html", "json", "browser", "url", "http" ]
    , h2 [] [ text "Resources" ]
    , ul []
        [ li [] [ a [ href "http://klaftertief.github.io/elm-search/" ] [ text "Fancy Search" ] ]
        , li [] [ a [ href "https://github.com/elm-lang/elm-package/blob/master/README.md" ] [ text "Using Packages" ] ]
        , li [] [ a [ href "/docs/TODO" ] [ text "API Design Guidelines" ] ]
        , li [] [ a [ href "/docs/TODO" ] [ text "Write great docs" ] ]
        , li [] [ a [ href "https://elm-lang.org" ] [ text "Elm Website" ] ]
        ]
    ]


viewPopularPackage : String -> Html Msg
viewPopularPackage project =
  li []
    [ a [ href (Href.toVersion "elm" project Nothing)
        ]
        [ span [ class "light" ] [ text "elm/" ]
        , text project
        ]
    ]
