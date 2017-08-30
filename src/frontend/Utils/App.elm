module Utils.App exposing
  ( Body, body, viewBody
  , link
  )

import Html exposing (Html, Attribute, a, div)
import Html.Attributes exposing (class, href)
import Html.Events exposing (preventDefaultOn)
import Json.Decode as D
import Route exposing (Route)



-- NODE


type Body msg =
  Body (List (Attribute msg)) (List (Html msg))


body : List (Attribute msg) -> List (Html msg) -> Body msg
body =
  Body


viewBody : Body msg -> Html msg
viewBody (Body attrs kids) =
  div (class "center" :: attrs) kids



-- LINKS


link : (Route -> msg) -> Route -> List (Attribute msg) -> List (Html msg) -> Html msg
link toMsg route attrs kids =
  a (attrs ++ [ href (Route.toUrl route), onRouteClick toMsg route ]) kids


onRouteClick : (Route -> msg) -> Route -> Attribute msg
onRouteClick toMsg route =
  preventDefaultOn "click" <|
    D.map4
      clickDecoder
      (D.succeed toMsg)
      (D.succeed route)
      (D.field "ctrlKey" D.bool)
      (D.field "metaKey" D.bool)


clickDecoder : (Route -> msg) -> Route -> Bool -> Bool -> (msg, Bool)
clickDecoder toMsg route ctrl meta =
  ( toMsg route, not ctrl && not meta )
