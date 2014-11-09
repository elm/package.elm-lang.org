module Component.ModuleDocs where

import Basics (..)
import Char
import ColorScheme as C
import Dict
import Debug
import Graphics.Element (..)
import JavaScript (..)
import JavaScript as JS
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
            |> Text.height 24
            |> Text.leftAligned

        header =
          container innerWidth 100 midLeft bigWords
    in
    flow down
    [ header
    , color C.lightGrey (spacer innerWidth 1)
    , spacer innerWidth 12
    , viewDocs innerWidth Dict.empty docs.comment
    ]


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
    List.concatMap (viewVar innerWidth documentation) vars
    ++ [viewProse innerWidth prose]


viewProse : Int -> String -> Element
viewProse innerWidth prose =
    width innerWidth (Markdown.toElement prose)


viewVar : Int -> Dict.Dict String String -> String -> [Element]
viewVar innerWidth documentation var =
    case Dict.get var documentation of
      Maybe.Nothing -> [] -- Debug.crash "everything should be in the dictionary"
      Maybe.Just str ->
        [ color C.lightGrey (spacer innerWidth 1)
        , container innerWidth 30 midLeft (Text.leftAligned (Text.monospace (Text.bold (Text.toText var) ++ Text.toText " : a -> b -> a")))
        , flow right
          [ spacer 40 10
          , viewProse (innerWidth - 40) str
          ]
        ]


-- MODEL OF DOCUMENTATION

type alias Documentation =
    { name : String
    , comment : String
    , aliases : [Alias]
    , unions : [Union]
    , values : [Value]
    }


documentation : Get Documentation
documentation =
    object5 Documentation
      ("name" := string)
      ("comment" := string)
      ("aliases" := list alias)
      ("types" := list union)
      ("values" := list value)


type alias Alias =
    { name : String
    , comment : String
    , args : [String]
    , tipe : Type
    }


alias : Get Alias
alias =
    object4 Alias
      ("name" := string)
      ("comment" := string)
      ("args" := list string)
      ("type" := tipe)


type alias Union =
    { name : String
    , comment : String
    , args : [String]
    , cases : [(String, [Type])]
    }


union : Get Union
union =
    object4 Union
      ("name" := string)
      ("comment" := string)
      ("args" := list string)
      ("cases" := list (tuple2 (,) string (list tipe)))


type alias Value =
    { name : String
    , comment : String
    , tipe : Type
    , assocPrec : Maybe.Maybe (String,Int)
    }


value : Get Value
value =
    object4 Value
      ("name" := string)
      ("comment" := string)
      ("type" := tipe)
      (maybe ("assoc-prec" := tuple2 (,) string int))


type Type
    = Lambda Type Type
    | Var String
    | Type String
    | App Type [Type]
    | Record [(String, Type)] (Maybe.Maybe Type)


tipe : Get Type
tipe =
    ("tag" := string) `andThen` specificType


specificType : String -> Get Type
specificType tag =
    case tag of
      "lambda" ->
          object2 Lambda
            ("in" := tipe)
            ("out" := tipe)

      "var" ->
          object1 Var ("name" := string)

      "type" ->
          object1 Type ("name" := string)

      "app" ->
          object2 App
            ("func" := tipe)
            ("args" := list tipe)

      "record" ->
          object2 Record
            ("fields" := list (tuple2 (,) string tipe))
            ("extension" := maybe tipe)

--      _ ->
--          error <| "unrecognized tag '" ++ tag ++ "' when getting a Type"
