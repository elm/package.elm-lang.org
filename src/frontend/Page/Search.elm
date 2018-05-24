module Page.Search exposing
  ( Model
  , Msg
  , update
  , view
  )

import Browser.Navigation as Navigation
import Html exposing (..)
import Html.Attributes exposing (autofocus, class, href, placeholder, style, value)
import Html.Events exposing (..)
import String

import Page.Search.Entry as Entry
import Route
import Utils.App as App
import Version



-- MODEL


type alias Model =
  { query : String
  , entries : List Entry.Entry
  }



-- UPDATE


type Msg
  = Push Route.Route
  | Query String


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
  case msg of
    Push route ->
      ( model, Navigation.pushUrl (Route.toUrl route) )

    Query query ->
      ( { model | query = query }, Cmd.none )



-- VIEW


view : Model -> App.Body Msg
view { entries, query } =
  App.body []
    [ div [ class "catalog" ]
        [ input
            [ placeholder "Search"
            , value query
            , onInput Query
            , autofocus True
            ]
            []
        , div [] (List.map viewEntry (Entry.search query entries))
        ]
    , viewSidebar
    ]



-- VIEW ENTRY


viewEntry : Entry.Entry -> Html Msg
viewEntry ({ author, project, summary } as entry) =
  div [ class "pkg-summary" ]
    [ div []
        [ h1 []
            [ App.link Push (Route.latest author project) []
                [ span [ class "light" ] [ text (author ++ "/") ]
                , text project
                ]
            ]
        , viewExactVersions entry
        ]
    , p [ class "pkg-summary-desc" ] [ text summary ]
    ]


viewExactVersions : Entry.Entry -> Html Msg
viewExactVersions { author, project, versions } =
  let
    exactVersion vsn =
      App.link Push (Route.exactly author project vsn) [] [ text (Version.toString vsn) ]

    allVersions =
      List.intersperse (text " … ") (List.map exactVersion versions)
      ++
      [ text " — "
      , App.link Push (Route.Package author project) [] [ text "Overview" ]
      ]
  in
  span [ class "pkg-summary-hints" ] <|
    case versions of
      Version.Version 1 0 0 :: _ ->
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
        , li [] [ App.link Push Route.Guidelines [] [ text "API Design Guidelines" ] ]
        , li [] [ App.link Push Route.DocsHelp [] [ text "Write great docs" ] ]
        , li [] [ a [ href "https://elm-lang.org" ] [ text "Elm Website" ] ]
        ]
    ]


viewPopularPackage : String -> Html Msg
viewPopularPackage project =
  li []
    [ App.link
        Push
        (Route.latest "elm" project)
        []
        [ span [ class "light" ] [ text "elm/" ]
        , text project
        ]
    ]
