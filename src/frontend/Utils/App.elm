module Utils.App exposing
  ( Update(..)
  , link
  )

import Html exposing (Html, Attribute, a)
import Html.Attributes exposing (href)
import Html.Events exposing (preventDefaultOn)
import Json.Decode as D
import Route exposing (Route)



-- UPDATES


type Update a
  = Goto Route
  | Update a



-- LINKS


link : (Route -> msg) -> Route -> List (Attribute msg) -> List (Html msg) -> Html msg
link toMsg route attrs kids =
  a (attrs ++ [ href (Route.toString route), onRouteClick toMsg route ]) kids


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
