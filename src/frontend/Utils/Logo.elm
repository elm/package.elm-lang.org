module Utils.Logo exposing (logo)


import Html
import Svg exposing (..)
import Svg.Attributes as A exposing (..)



-- ELM LOGO


logo : Int -> Html.Html msg
logo n =
  svg
    [ height (String.fromInt n)
    , viewBox "0 0 600 600"
    ]
    [ shape "#5A6378" "0,20 280,300 0,580"
    , shape "#60B5CC" "20,600 300,320 580,600"
    , shape "#60B5CC" "320,0 600,0 600,280"
    , shape "#7FD13B" "20,0 280,0 402,122 142,122"
    , shape "#F0AD00" "170,150 430,150 300,280"
    , shape "#7FD13B" "320,300 450,170 580,300 450,430"
    , shape "#F0AD00" "470,450 600,320 600,580"
    ]


shape : String -> String -> Svg msg
shape color coordinates =
  polygon [ fill color, points coordinates ] []
