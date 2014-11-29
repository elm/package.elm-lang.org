module Component.DropDown where

import Html (..)
import Html.Attributes (..)
import Html.Events (..)
import Json.Decode (at, int)
import List
import LocalChannel as LC


dropdown : LC.LocalChannel String -> String -> List String -> Html
dropdown updates current options =
  select
    [ style [ ("display","inline-block"), ("vertical-align","middle") ]
    , on "change"
        (at ["target","selectedIndex"] int)
        (LC.send updates << nth options)
    ]
    (List.map (viewOption current) options)


viewOption : String -> String -> Html
viewOption current string =
  option
    [ value string, selected (current == string) ]
    [ text string ]


nth : List a -> Int -> a
nth (x::rest) n =
  if n == 0 then x else nth rest (n-1)
