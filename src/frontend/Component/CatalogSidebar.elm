module Component.CatalogSidebar exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model = ()



-- INIT


init : (Model, Cmd Msg)
init =
  ( ()
  , Cmd.none
  )



-- UPDATE


type Msg = Act


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Act ->
      ( model
      , Cmd.none
      )



-- VIEW


(=>) = (,)


view : Model -> Html Msg
view model =
  div [class "catalog-sidebar"]
    [ h2 [] [ text "Resources" ]
    , ul []
        [ li [] [ a [ href "https://github.com/elm-lang/elm-package/blob/master/README.md" ] [ text "Using Packages" ] ]
        , li [] [ a [ href "/help/design-guidelines" ] [ text "API Design Guidelines" ] ]
        , li [] [ a [ href "/help/documentation-format" ] [ text "Write great docs" ] ]
        , li [] [ a [ href "/help/docs-preview" ] [ text "Preview your docs" ] ]
        , li [] [ a [ href "http://elm-lang.org" ] [ text "Elm Website" ] ]
        ]
    , h2 [] [ text "Popular Packages" ]
    , ul []
        [ pkgBlock "General" generalPackages
        , pkgBlock "Rendering" renderingPackages
        , pkgBlock "Effects" effectPackages
        ]
    ]


pkgBlock : String -> List (String, String) -> Html msg
pkgBlock title pkgs =
  li []
    [ text title
    , ul [] (List.map pkgBlockItem pkgs)
    ]


pkgBlockItem : (String, String) -> Html msg
pkgBlockItem (project, niceName) =
  li []
    [ a [ href ("/packages/" ++ project ++ "/latest") ] [ text niceName ]
    ]


generalPackages : List (String, String)
generalPackages =
  [ "elm-lang/core" => "core"
  ]


renderingPackages : List (String, String)
renderingPackages =
  [ "elm-lang/html" => "html"
  , "elm-lang/svg" => "svg"
  , "evancz/elm-markdown" => "markdown"
  ]


effectPackages : List (String, String)
effectPackages =
  [ "evancz/elm-http" => "http"
  , "elm-lang/geolocation" => "geolocation"
  , "elm-lang/navigation" => "navigation"
  , "elm-lang/page-visibility" => "page-visibility"
  , "elm-lang/websocket" => "websocket"
  ]
