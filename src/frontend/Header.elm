module Header where

import Effects as Fx exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)


-- MODEL

type alias Model =
    { user : String
    , package : String
    , version : String
    , allVersions : List String
    , focus : Maybe String
    }


dummy : Model
dummy =
  Model "evancz" "core" "3.0.0" ["3.0.0", "2.0.0", "1.0.0"] Nothing


-- UPDATE

update : a -> Model -> (Model, Effects a)
update action model =
  (model, Fx.none)


-- VIEW

(=>) = (,)


view : Signal.Address a -> Model -> Html
view _ model =
  div []
    [ center "rgb(17, 132, 206)" [ headerLinks model ]
    , center "#eeeeee" [ versionWarning model ]
    ]


-- header links

headerLinks model =
  h1 [ class "header" ] <|
    [ logo, packageList, spacey "/", userLink model, spacey "/", packageLink model, spacey "/", versionLink model ]
    ++
    case model.focus of
      Nothing ->
        []

      Just moduleName ->
        [ spacey "/", text moduleName ]


center color kids =
  div [ style [ "background-color" => color ] ]
    [ div [ class "center" ] kids
    ]


spacey token =
  span [ class "spacey-char" ] [ text token ]


logo =
  a [ href "/", class "logo" ]
    [ img [ src "/assets/elm_logo.svg" ] []
    , text "elm"
    ]


headerLink url words =
  a [ href url, style [ "color" => "white" ] ]
    [ text words ]


packageList =
  headerLink "/packages" "packages"


userLink model =
  headerLink
    ("https://github.com/" ++ model.user)
    model.user


packageLink model =
  headerLink
    ("/packages/" ++ model.user ++ "/" ++ model.package)
    model.package


versionLink model =
  headerLink
    ("/packages/" ++ model.user ++ "/" ++ model.package ++ "/" ++ model.version)
    model.version


-- version warnings

versionWarning model =
  p [ class "version-warning" ] <|
    case List.head model.allVersions of
      Nothing ->
          []

      Just latestVersion ->
          if model.version == latestVersion then
              []

          else
              [ text "Warning! The latest version of this package is "
              , a [ href ("/packages/" ++ model.user ++ "/" ++ model.package ++ "/" ++ latestVersion) ]
                  [ text latestVersion ]
              ]

