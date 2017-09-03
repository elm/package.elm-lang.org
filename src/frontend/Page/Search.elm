module Page.Search exposing
  ( Model
  , Msg
  , update
  , view
  )

import Browser.History as History
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
      ( model, History.push (Route.toUrl route) )

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
viewEntry ({ name, user, project, summary } as entry) =
  div [ class "pkg-summary" ]
    [ div []
        [ h1 [] [ App.link Push (Route.latest user project) [] [ text name ] ]
        , viewExactVersions entry
        ]
    , p [ class "pkg-summary-desc" ] [ text summary ]
    ]


viewExactVersions : Entry.Entry -> Html Msg
viewExactVersions { user, project, versions } =
  let
    exactVersion vsn =
      App.link Push (Route.exactly user project vsn) [] [ text (Version.toString vsn) ]

    allVersions =
      List.intersperse (text " … ") (List.map exactVersion versions)
      ++
      [ text " — "
      , App.link Push (Route.Package user project) [] [ text "Overview" ]
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
    [ h2 [] [ text "Resources" ]
    , ul []
        [ li [] [ a [ href "http://klaftertief.github.io/elm-search/" ] [ text "Fancy Search" ] ]
        , li [] [ a [ href "https://github.com/elm-lang/elm-package/blob/master/README.md" ] [ text "Using Packages" ] ]
        , li [] [ a [ href "/help/design-guidelines" ] [ text "API Design Guidelines" ] ]
        , li [] [ a [ href "/help/documentation-format" ] [ text "Write great docs" ] ]
        , li [] [ a [ href "http://elm-lang.org" ] [ text "Elm Website" ] ]
        ]
    , h2 [] [ text "Popular Packages" ]
    , ul []
        [ pkgBlock "General" generalPackages
        , pkgBlock "Rendering" renderingPackages
        , pkgBlock "Effects" effectPackages
        , pkgBlock "User Input" inputPackages
        ]
    ]


pkgBlock : String -> List (String, String, String) -> Html Msg
pkgBlock title pkgs =
  li []
    [ text title
    , ul [] (List.map pkgBlockItem pkgs)
    ]


pkgBlockItem : (String, String, String) -> Html Msg
pkgBlockItem (user, project, niceName) =
  li []
    [ App.link Push (Route.latest user project) [] [ text niceName ]
    ]


generalPackages : List (String, String, String)
generalPackages =
  [ ( "elm-lang", "core", "core" )
  , ( "elm-lang", "http", "http" )
  ]


renderingPackages : List (String, String, String)
renderingPackages =
  [ ( "elm-lang", "html", "html" )
  , ( "elm-lang", "svg", "svg" )
  , ( "evancz", "elm-markdown", "markdown" )
  ]


effectPackages : List (String, String, String)
effectPackages =
  [ ( "elm-lang", "dom", "dom" )
  , ( "elm-lang", "navigation", "navigation" )
  , ( "elm-lang", "geolocation", "geolocation" )
  , ( "elm-lang", "page-visibility", "page-visibility" )
  , ( "elm-lang", "websocket", "websocket" )
  ]


inputPackages : List (String, String, String)
inputPackages =
  [ ( "elm-lang", "mouse", "mouse" )
  , ( "elm-lang", "window", "window" )
  , ( "elm-lang", "keyboard", "keyboard" )
  ]
