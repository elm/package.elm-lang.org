module Component.ModuleDocs where

import Basics (..)
import Char
import ColorScheme as C
import Dict
import Debug
import Graphics.Element (..)
import List
import List ((::), (++))
import Markdown
import Maybe
import Regex
import String
import Text


view : Int -> String -> String -> Documentation -> Element
view innerWidth user package docs =
    let bigWords =
          Text.toText (user ++ " / " ++ package ++ " / " ++ docs.name)
            |> Text.height 40
            |> Text.leftAligned

        header =
          container innerWidth 100 midLeft bigWords
    in
    flow down
    [ header
    , color C.lightGrey (spacer innerWidth 1)
    , spacer innerWidth 12
    , viewDocs innerWidth documentation docs.comment
    ]


dummyDocs =
    Documentation
        "Basics"
        comment
        []
        []
        [ Value "identity" identityComment (Lambda (Var "a") (Var "a")) Maybe.Nothing ]


comment = """
basic functions for stuff

@docs identity, always

basic functions for stuff

@docs identity, always
"""

documentation : Dict.Dict String String
documentation =
    Dict.fromList [("identity", identityComment), ("always", alwaysComment)]

identityComment = """
Function that returns the argument unaltered

    identity 42 == 42
"""

alwaysComment = """
Function that always returns the same value

    always 42 "hats" == 42
    always 42 123456 == 42
    always 42 [3..8] == 42
"""


viewDocs : Int -> Dict.Dict String String -> String  -> Element
viewDocs innerWidth documentation comment =
  let (prose :: chunks) =
        String.split "\n@docs " comment

      varProsePairs : [([String], String)]
      varProsePairs =
        List.map (extractVars [] Space) chunks
  in
      flow down <|
        viewProse innerWidth prose
        :: List.concatMap (viewPair innerWidth documentation) varProsePairs


type ExtractState = Variable String | Comma | Space

extractVars : [String] -> ExtractState -> String -> ([String], String)
extractVars vars state chunk =
  case String.uncons chunk of
    Maybe.Nothing ->
      case state of
        Variable x ->
          (List.reverse (String.reverse x :: vars), "")
        _ ->
          (List.reverse vars, "")

    Maybe.Just (c, rest) ->
      case state of
        Variable x ->
          if  | isVarChar c ->
                  extractVars vars (Variable (String.cons c x)) rest
              | String.any ((==) c) " \n\t\r" ->
                  extractVars (String.reverse x :: vars) Comma rest
              | c == ',' ->
                  extractVars (String.reverse x :: vars) Space rest
              | otherwise ->
                  (List.reverse (String.reverse x :: vars), chunk)

        Comma ->
          if  | String.any ((==) c) " \n\t\r" ->
                  extractVars vars Comma rest
              | c == ',' ->
                  extractVars vars Space rest
              | otherwise ->
                  (List.reverse vars, chunk)

        Space ->
          if  | isVarChar c ->
                  extractVars vars (Variable (String.cons c "")) rest
              | String.any ((==) c) " \n\t\r" ->
                  extractVars vars Space rest
              | otherwise ->
                  (List.reverse vars, chunk)


isVarChar : Char -> Bool
isVarChar c =
    Char.isLower c || Char.isUpper c || Char.isDigit c || c == '_' || c == '\''


docsPattern : Regex.Regex
docsPattern =
    Regex.regex "(.*)\n@docs\\s+([a-zA-Z0-9_']+(?:,\\s*[a-zA-Z0-9_']+)*)"


viewPair : Int -> Dict.Dict String String -> ([String], String) -> [Element]
viewPair innerWidth documentation (vars, prose) =
    List.map (viewVar innerWidth documentation) vars
    ++ [viewProse innerWidth prose]


viewProse : Int -> String -> Element
viewProse innerWidth prose =
    width innerWidth (Markdown.toElement prose)


viewVar : Int -> Dict.Dict String String -> String -> Element
viewVar innerWidth documentation var =
    case Dict.get var documentation of
      Maybe.Just str -> viewProse innerWidth str
      Maybe.Nothing -> Debug.crash "everything should be in the dictionary"


-- MODEL OF DOCUMENTATION

type alias Documentation =
    { name : String
    , comment : String
    , aliases : [Alias]
    , unions : [Union]
    , values : [Value]
    }


type alias Alias =
    { name : String
    , comment : String
    , args : [String]
    , tipe : Type
    }


type alias Union =
    { name : String
    , comment : String
    , args : [String]
    , cases : [(String, [Type])]
    }


type alias Value =
    { name : String
    , comment : String
    , tipe : Type
    , assocPrec : Maybe.Maybe (String,Int)
    }


type Type
    = Lambda Type Type
    | Var String
    | Type String
    | App Type [Type]
    | Record [(String, Type)] (Maybe.Maybe Type)