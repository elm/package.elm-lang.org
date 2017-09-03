module Page.Search.Entry exposing
  ( Entry
  , search
  , decoder
  )


import Json.Decode as D
import String
import Version



-- ENTRY


type alias Entry =
  { name : String
  , user : String
  , project : String
  , summary : String
  , license : String
  , versions : List Version.Version
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
    (D.field "versions" (D.list Version.decoder))


splitName : String -> D.Decoder (String -> String -> List Version.Version -> Entry)
splitName name =
  case String.split "/" name of
    [user, project] ->
      D.succeed (Entry name user project)

    _ ->
      D.fail ("Ran into an invalid package name: " ++ name)
