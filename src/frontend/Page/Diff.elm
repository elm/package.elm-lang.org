module Page.Diff exposing
  ( Model
  , view
  , toTitle
  )


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (..)
import Release
import Route
import Url
import Utils.App as App
import Utils.Markdown as Markdown
import Version



-- MODEL


type alias Model =
  { user : String
  , project : String
  , releases : List Release.Release
  }



-- TO TITLE


toTitle : Model -> String
toTitle model =
  model.project



-- VIEW


view : Model -> App.Body Route.Route
view { user, project, releases } =
  App.body
    [ class "pkg-overview"
    ]
    [ h1 [] [ text "Published Versions" ]
    , releases
        |> List.sortBy .time
        |> List.map .version
        |> viewReleases user project
        |> p []
    ]


viewReleases : String -> String -> List Version.Version -> List (Html Route.Route)
viewReleases user project versions =
  case versions of
    v1 :: ((v2 :: _) as vs) ->
      let
        attrs =
          if Version.sameMajor v1 v2 then [] else [ bold ]
      in
      readmeLink user project v1 attrs :: text ", " :: viewReleases user project vs

    r0 :: [] ->
      [ readmeLink user project r0 [ bold ] ]

    [] ->
      []


bold : Attribute msg
bold =
  style "font-weight" "bold"


readmeLink : String -> String -> Version.Version -> List (Attribute Route.Route) -> Html Route.Route
readmeLink user project version attrs =
  App.link
    identity
    (Route.Version user project (Route.Exactly version) Route.Readme)
    attrs
    [ text (Version.toString version) ]
