module Component.DropDown where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (at, int)


dropdown : Signal.Address String -> String -> List String -> Html
dropdown address current options =
  div
    [ style [("text-align", "center")] ]
    [ select
        [ style [ ("display","inline-block"), ("vertical-align","middle") ]
        , on "change"
            (at ["target","selectedIndex"] int)
            (Signal.message address << nth options)
        ]
        (List.map (viewOption current) options)
    ]


viewOption : String -> String -> Html
viewOption current string =
  option
    [ value string, selected (current == string) ]
    [ text string ]


nth : List a -> Int -> a
nth (x::rest) n =
  if n == 0 then x else nth rest (n-1)
