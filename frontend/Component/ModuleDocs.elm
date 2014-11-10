module Component.ModuleDocs where

import Basics (..)
import Dict
import Debug
import Graphics.Element (..)
import List
import List ((::))
import Markdown
import Maybe
import Regex
import String
import Text

import ColorScheme as C
import Component.Documentation as D


view : Int -> String -> String -> D.Documentation -> Element
view innerWidth user package docs =
    let bigWords =
          Text.fromString (user ++ " / " ++ package ++ " / " ++ docs.name)
            |> Text.height 24
            |> Text.leftAligned

        header =
          container innerWidth 100 midLeft bigWords
    in
    flow down
    [ header
    , color C.lightGrey (spacer innerWidth 1)
    , spacer innerWidth 12
    , viewDocs innerWidth (D.toDocDict docs) docs.comment
    ]


viewDocs : Int -> Dict.Dict String Element -> String  -> Element
viewDocs innerWidth documentation comment =
  let (prose :: chunks) =
        String.split "\n@docs " comment

      varProsePairs : [([String], String)]
      varProsePairs =
        List.map extractVars chunks
  in
      flow down <|
        viewProse innerWidth prose
        :: List.concatMap (viewPair innerWidth documentation) varProsePairs


extractVars : String -> ([String], String)
extractVars rawChunk =
  let chunk = String.trimLeft rawChunk
  in
      case String.uncons chunk of
        Maybe.Nothing ->
          ([], "")

        Maybe.Just (c, subchunk) ->
          if  | D.isVarChar c ->
                  let (var, rest) = takeWhile D.isVarChar chunk
                      (vars, nextChunk) = extractCommaThenVars rest
                  in
                      (var :: vars, nextChunk)

              | otherwise ->
                  let (op, rest) = takeWhile ((/=) ')') subchunk
                      (vars, nextChunk) = extractCommaThenVars (String.dropLeft 1 rest)
                  in
                      (op :: vars, nextChunk)


extractCommaThenVars : String -> ([String], String)
extractCommaThenVars rawChunk =
  let chunk = String.trimLeft rawChunk
  in
      case String.uncons chunk of
        Maybe.Just (',', subchunk) ->
          extractVars subchunk

        _ -> ([], chunk)


takeWhile : (Char -> Bool) -> String -> (String, String)
takeWhile pred chunk =
  case String.uncons chunk of
    Maybe.Nothing -> ("", "")
    Maybe.Just (c, rest) ->
      if  | pred c ->
              let (result, chunk') = takeWhile pred rest
              in
                  (String.cons c result, chunk')

          | otherwise ->
              ("", chunk)


docsPattern : Regex.Regex
docsPattern =
    Regex.regex "(.*)\n@docs\\s+([a-zA-Z0-9_']+(?:,\\s*[a-zA-Z0-9_']+)*)"


viewPair : Int -> Dict.Dict String Element -> ([String], String) -> [Element]
viewPair innerWidth documentation (vars, prose) =
    List.concatMap (viewVar innerWidth documentation) vars
    ++ [viewProse innerWidth prose]


viewProse : Int -> String -> Element
viewProse innerWidth prose =
    width innerWidth (Markdown.toElement prose)


viewVar : Int -> Dict.Dict String Element -> String -> [Element]
viewVar innerWidth documentation var =
    case Dict.get var documentation of
      Maybe.Nothing ->
        let msg =
              Text.concat
              [ Text.fromString "There is some problem with the docs for '"
              , Text.monospace (Text.fromString var)
              , Text.fromString "', please inform the package author."
              ]
        in
            [ width innerWidth (Text.leftAligned msg) ]

      Maybe.Just element ->
        [ element ]
        {--
        [ color C.lightGrey (spacer innerWidth 1)
        , container innerWidth 30 midLeft (Text.leftAligned (Text.monospace (Text.bold (Text.fromString var) ++ Text.fromString " : a -> b -> a")))
        , flow right
          [ spacer 40 10
          , viewProse (innerWidth - 40) str
          ]
        ]
        --}