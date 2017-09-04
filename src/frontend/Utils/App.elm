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
  a (attrs ++ [ href (Route.toUrl route), onRouteClick (toMsg route) ]) kids


onRouteClick : msg -> Attribute msg
onRouteClick msg =
  preventDefaultOn "click" <|
    D.andThen (clickDecoder msg) modifiers


modifiers : D.Decoder Bool
modifiers =
  D.map2 (||)
    (D.field "ctrlKey" D.bool)
    (D.field "metaKey" D.bool)


clickDecoder : msg -> Bool -> D.Decoder (msg, Bool)
clickDecoder msg isModified =
  if isModified then
    D.fail ""
  else
    D.succeed ( msg, True )
