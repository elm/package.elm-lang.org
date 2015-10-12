module Navigator where

import Effects as Fx exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)

import Header


-- MODEL

type alias Model =
    { readme : Maybe String
    , modules : List String
    , context : Header.Model
    }


dummy : Model
dummy =
  Model "hello, how are you?" ["Task", "Result"] Header.dummy


-- UPDATE

update : a -> Model -> (Model, Effects a)
update action model =
  (model, Fx.none)


-- VIEW

(=>) = (,)


view : Signal.Address a -> Model -> Html
view _ model =
  div []
    [ div
        [ style [ "padding" => "10px 0", "background-color" => "#401155", "color" => "white" ] ]
        [ headerLinks model ]
    , p [ style [ "text-align" => "center", "background-color" => "#cccccc" ] ]
        (versionWarning model)
    ]


-- header links

headerLinks model =
  case model.focus of
    Nothing ->
      h1 [headerStyle]
        [ userName model, slash, packageName model ]

    Just moduleName ->
      h1 [headerStyle]
        [ userName model, slash, packageName model, slash, text moduleName ]


headerStyle =
  style [ "margin" => "0" ]


slash =
  span [ style [ "margin" => "0 10px" ] ] [ text "/" ]


userName model =
  a [ href ("https://github.com/" ++ model.user) ]
    [ text model.user ]


packageName model =
  a [ href ("/packages/" ++ model.user ++ "/" ++ model.package) ]
    [ text model.package ]


-- version warnings

versionWarning model =
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

