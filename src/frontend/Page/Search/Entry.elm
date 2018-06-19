module Page.Search.Entry exposing
  ( Entry
  , search
  , decoder
  )


import Elm.Version as V
import Json.Decode as D



-- ENTRY


type alias Entry =
  { name : String
  , author : String
  , project : String
  , summary : String
  , license : String
  , versions : List V.Version
  }



-- SEARCH


search : String -> List Entry -> List Entry
search query entries =
  let
    queryTerms =
      String.words (String.toLower query)

    matchesAllTerms entry =
      let
        lowerName =
          String.toLower entry.name

        lowerSummary =
          String.toLower entry.summary

        matchesTerm term =
          String.contains term lowerName
          || String.contains term lowerSummary
      in
      List.all matchesTerm queryTerms
  in
  List.filter matchesAllTerms entries



-- DECODER


decoder : D.Decoder Entry
decoder =
  D.map4 (\f a b c -> f a b c)
    (D.field "name" (D.andThen splitName D.string))
    (D.field "summary" D.string)
    (D.field "license" D.string)
    (D.field "versions" (D.list V.decoder))


splitName : String -> D.Decoder (String -> String -> List V.Version -> Entry)
splitName name =
  case String.split "/" name of
    [author, project] ->
      D.succeed (Entry name author project)

    _ ->
      D.fail ("Ran into an invalid package name: " ++ name)
