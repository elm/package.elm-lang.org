module Component.CatalogSidebar where

import Effects as Fx exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model = ()



-- INIT


init : (Model, Effects Action)
init =
  ( ()
  , Fx.none
  )



-- UPDATE


type Action = Act


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Act ->
      ( model
      , Fx.none
      )



-- VIEW


(=>) = (,)


view : Signal.Address Action -> Model -> Html
view addr model =
  div [class "catalog-sidebar"]
    [ h2 [] [ text "Resources" ]
    , ul []
        [ li [] [ a [ href "https://github.com/elm-lang/elm-package/blob/master/README.md" ] [ text "Using Packages" ] ]
        , li [] [ a [ href "/help/design-guidelines" ] [ text "API Design Guidelines" ] ]
        , li [] [ a [ href "/help/documentation-format" ] [ text "Write great docs" ] ]
        , li [] [ a [ href "/help/docs-preview" ] [ text "Preview your docs" ] ]
        , li [] [ a [ href "/search" ] [ text "API Search" ] ]
        , li [] [ a [ href "http://elm-lang.org" ] [ text "Elm Website" ] ]
        ]
    , h2 [] [ text "Popular Packages" ]
    , ul []
        [ pkgBlock "General" generalPackages
        , pkgBlock "Rendering" renderingPackages
        , pkgBlock "Effects" effectsPackages
        ]
    ]


pkgBlock : String -> List (String, String) -> Html
pkgBlock title pkgs =
  li []
    [ text title
    , ul [] (List.map pkgBlockItem pkgs)
    ]


pkgBlockItem : (String, String) -> Html
pkgBlockItem (user, project) =
  li []
    [ a [ href ("/packages/" ++ user ++ "/" ++ project ++ "/latest") ] [ text project ]
    ]


generalPackages : List (String, String)
generalPackages =
  [ "elm-lang" => "core"
  ]


renderingPackages : List (String, String)
renderingPackages =
  [ "evancz" => "elm-html"
  , "evancz" => "elm-svg"
  , "evancz" => "elm-markdown"
  , "elm-community" => "elm-webgl"
  ]


effectsPackages : List (String, String)
effectsPackages =
  [ "evancz" => "elm-http"
  , "evancz" => "start-app"
  , "evancz" => "elm-effects"
  ]
