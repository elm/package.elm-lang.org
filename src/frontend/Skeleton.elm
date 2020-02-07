module Skeleton exposing
  ( Details
  , Warning(..)
  , view
  , Segment
  , helpSegment
  , authorSegment
  , projectSegment
  , versionSegment
  , moduleSegment
  )


import Browser
import Elm.Version as V
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (..)
import Href
import Json.Decode as D
import Utils.Logo as Logo



-- NODE


type alias Details msg =
  { title : String
  , header : List Segment
  , warning : Warning
  , attrs : List (Attribute msg)
  , kids : List (Html msg)
  }


type Warning
  = NoProblems
  | NewerVersion String V.Version



-- SEGMENT


type Segment
  = Text String
  | Link String String


helpSegment : Segment
helpSegment =
  Text "help"


authorSegment : String -> Segment
authorSegment author =
  Text author


projectSegment : String -> String -> Segment
projectSegment author project =
  Link (Href.toProject author project) project


versionSegment : String -> String -> Maybe V.Version -> Segment
versionSegment author project version =
  Link (Href.toVersion author project version) (vsnToString version)


moduleSegment : String -> String -> Maybe V.Version -> String -> Segment
moduleSegment author project version moduleName =
  Link (Href.toModule author project version moduleName Nothing) moduleName


vsnToString : Maybe V.Version -> String
vsnToString maybeVersion =
  case maybeVersion of
    Nothing ->
      "latest"

    Just version ->
      V.toString version



-- VIEW


view : (a -> msg) -> Details a -> Browser.Document msg
view toMsg details =
  { title =
      details.title
  , body =
      [ viewHeader details.header
      , lazy viewWarning details.warning
      , Html.map toMsg <|
          div (class "center" :: details.attrs) details.kids
      , viewFooter
      ]
  }



-- VIEW HEADER


viewHeader : List Segment -> Html msg
viewHeader segments =
  div [class "header"]
    [ div [class "nav"]
        [ viewLogo
        , case segments of
            [] -> text ""
            _  -> h1 [] (List.intersperse slash (List.map viewSegment segments))
        ]
    ]



slash : Html msg
slash =
  span [ class "spacey-char" ] [ text "/" ]


viewSegment : Segment -> Html msg
viewSegment segment =
  case segment of
    Text string ->
      text string

    Link address string ->
      a [ href address ] [ text string ]



-- VIEW WARNING


viewWarning : Warning -> Html msg
viewWarning warning =
  div [ class "header-underbar" ] <|
    case warning of
      NoProblems ->
        []

      NewerVersion url version ->
        [ p [ class "version-warning" ]
            [ text "NOTE — the latest version is "
            , a [ href url ] [ text (V.toString version) ]
            ]
        ]



-- VIEW FOOTER


viewFooter : Html msg
viewFooter =
  div [class "footer"]
    [ text "All code for this site is open source and written in Elm. "
    , a [ class "grey-link", href "https://github.com/elm/package.elm-lang.org/" ] [ text "Check it out" ]
    , text "! — © 2012-present Evan Czaplicki"
    ]



-- VIEW LOGO


viewLogo : Html msg
viewLogo =
  a [ href "/"
    , style "text-decoration" "none"
    , style "margin-right" "32px"
    , style "display" "flex"
    , style "align-items" "center"
    ]
    [ Logo.logo 32
    , div
        [ style "padding-left" "8px" ]
        [ div
            [ style "line-height" "24px"
            , style "font-size" "30px"
            ]
            [ text "elm" ]
        , div
            [ style "font-size" "12px"
            ]
            [ text "packages" ]
        ]
    ]
