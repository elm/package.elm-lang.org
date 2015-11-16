module Page.NotFound where

import Html exposing (..)
import Html.Attributes exposing (style)

import Component.Header as Header
import Route


(=>) = (,)


main =
  Header.view (Signal.mailbox ()).address (Header.Model (Route.Packages Nothing))
    [ div
        [ style
            [ "height" => "100%"
            , "text-align" => "center"
            , "color" => "#9A9A9A"
            , "padding" => "6em 0"
            ]
        ]
        [ div [ style ["font-size" => "12em"] ] [ text "404" ]
        , div [ style ["font-size" => "3em"] ] [ text "Page not found" ]
        ]
    ]