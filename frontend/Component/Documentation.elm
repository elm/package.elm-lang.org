module Component.Documentation where

import Char
import Color
import Dict
import Graphics.Element exposing (..)
import Json.Decode exposing (..)
import Markdown
import Regex
import String
import Text

import ColorScheme as C


type alias DocDict =
    Dict.Dict String (Text.Text, Maybe (String, Int), String)


toDocDict : List String -> Documentation -> DocDict
toDocDict modules docs =
  let toPairs view getAssocPrec entries =
          List.map (\entry -> (entry.name, (view entry, getAssocPrec entry, entry.comment))) entries
  in
      Dict.fromList <|
        toPairs (viewAlias modules) (always Nothing) docs.aliases
        ++ toPairs (viewUnion modules) (always Nothing) docs.unions
        ++ toPairs (viewValue modules) .assocPrec docs.values


-- MODEL

type alias Documentation =
    { name : String
    , comment : String
    , aliases : List Alias
    , unions : List Union
    , values : List Value
    }


documentation : Decoder Documentation
documentation =
  object5 Documentation
    ("name" := string)
    ("comment" := string)
    ("aliases" := list alias)
    ("types" := list union)
    ("values" := list value)


valueList : Decoder (String, List String)
valueList =
  let
    nameList =
      list ("name" := string)

    allNames =
      object3 (\x y z -> x ++ y ++ z)
        ("aliases" := nameList)
        ("types" := nameList)
        ("values" := nameList)
  in
    object2 (,) ("name" := string) allNames


type alias Alias =
    { name : String
    , comment : String
    , args : List String
    , tipe : Type
    }


alias : Decoder Alias
alias =
  object4 Alias
    ("name" := string)
    ("comment" := string)
    ("args" := list string)
    ("type" := string)


type alias Union =
    { name : String
    , comment : String
    , args : List String
    , cases : List (String, List Type)
    }


union : Decoder Union
union =
  object4 Union
    ("name" := string)
    ("comment" := string)
    ("args" := list string)
    ("cases" := list (tuple2 (,) string (list string)))


type alias Value =
    { name : String
    , comment : String
    , tipe : Type
    , assocPrec : Maybe (String,Int)
    }


value : Decoder Value
value =
  object4 Value
    ("name" := string)
    ("comment" := string)
    ("type" := string)
    assocPrec


assocPrec : Decoder (Maybe (String, Int))
assocPrec =
  maybe <|
    object2 (,)
      ("associativity" := string)
      ("precedence" := int)


type alias Type = String


-- VIEW

options : Markdown.Options
options =
  let
    defaults = Markdown.defaultOptions
  in
    { defaults | sanitize <- True }


viewEntry : Int -> String -> (Text.Text, Maybe (String, Int), String) -> Element
viewEntry innerWidth name (annotation, maybeAssocPrec, comment) =
  let
    rawAssocPrec =
      case maybeAssocPrec of
        Nothing -> empty
        Just (assoc, prec) ->
          assoc ++ "-associative / precedence " ++ toString prec
            |> Text.fromString
            |> Text.height 12
            |> rightAligned

    assocPrecWidth =
      widthOf rawAssocPrec + 20

    assocPrec =
      container assocPrecWidth (min annotationHeight 24) middle rawAssocPrec

    annotationText =
      leftAligned (Text.monospace annotation)
        |> width annotationWidth

    annotationPadding = 10

    annotationWidth =
      innerWidth - annotationPadding - assocPrecWidth

    annotationHeight =
      heightOf annotationText + 8

    commentElement =
      if String.isEmpty comment
          then empty
          else
              flow right
              [ spacer 40 1
              , width (innerWidth - 40) (Markdown.toElementWith options comment)
              ]

    annotationBar =
      flow right
      [ spacer annotationPadding annotationHeight
      , container annotationWidth annotationHeight midLeft annotationText
      , assocPrec
      ]
  in
    flow down
      [ tag name (color C.mediumGrey (spacer innerWidth 1))
      , annotationBar
      , commentElement
      , spacer innerWidth 50
      ]


-- VIEW ALIASES

viewAlias : List String -> Alias -> Text.Text
viewAlias modules alias =
  Text.concat
    [ green "type alias "
    , Text.link ("#" ++ alias.name) (Text.bold (Text.fromString alias.name))
    , Text.fromString (String.concat (List.map ((++) " ") alias.args))
    , green " = "
    , case String.uncons alias.tipe of
        Just ('{', _) ->
          viewRecordType modules alias.tipe

        _ ->
          typeToText modules alias.tipe
    ]


-- VIEW UNIONS

viewUnion : List String -> Union -> Text.Text
viewUnion modules union =
  let
    seperators =
      green "\n    = "
      :: List.repeat (List.length union.cases - 1) (green "\n    | ")
  in
    Text.concat
      [ green "type "
      , Text.link ("#" ++ union.name) (Text.bold (Text.fromString union.name))
      , Text.fromString (String.concat (List.map ((++) " ") union.args))
      , Text.concat (List.map2 (++) seperators (List.map (viewCase modules) union.cases))
      ]


viewCase : List String -> (String, List Type) -> Text.Text
viewCase modules (tag, args) =
  List.map (viewArg modules) args
    |> (::) (Text.fromString tag)
    |> List.intersperse (Text.fromString " ")
    |> Text.concat


viewArg : List String -> String -> Text.Text
viewArg modules tipe =
  let
    (Just (c,_)) =
      String.uncons tipe
  in
    if c == '(' || c == '{' || not (String.contains " " tipe) then
      typeToText modules tipe
    else
      typeToText modules ("(" ++ tipe ++ ")")


-- VIEW VALUES

viewValue : List String -> Value -> Text.Text
viewValue modules value =
  Text.concat
    [ Text.link ("#" ++ value.name) (Text.bold (viewVar value.name))
    , viewFunctionType modules value.tipe
    ]


viewVar : String -> Text.Text
viewVar str =
  Text.fromString <|
    case String.uncons str of
      Nothing ->
        str

      Just (c, _) ->
        if isVarChar c then str else "(" ++ str ++ ")"


isVarChar : Char -> Bool
isVarChar c =
  Char.isLower c || Char.isUpper c || Char.isDigit c || c == '_' || c == '\''


-- VIEW TYPES

viewRecordType : List String -> String -> Text.Text
viewRecordType modules tipe =
  splitRecord tipe
    |> List.map (Text.append (Text.fromString "\n    ") << typeToText modules)
    |> Text.concat


viewFunctionType : List String -> Type -> Text.Text
viewFunctionType modules tipe =
  if String.length (dropQualifiers tipe) < 80 then
    green " : " ++ typeToText modules tipe
  else
    let
      parts =
        splitArgs tipe

      seperators =
        green "\n    :  "
        :: List.repeat (List.length parts - 1) (green "\n    ->")
    in
      Text.concat (List.map2 (\sep -> Text.append sep << typeToText modules) seperators parts)


-- TYPE TO TEXT

typeToText : List String -> String -> Text.Text
typeToText modules =
  replaceMap " " (Text.fromString " ")
    <| replaceMap "," (green ",")
    <| replaceMap "(" (Text.fromString "(")
    <| replaceMap ")" (Text.fromString ")")
    <| replaceMap "{" (Text.fromString "{")
    <| replaceMap "}" (Text.fromString "}")
    <| replaceMap "->" (green "->")
    <| replaceMap ":" (green ":")
    <| linkQualified modules


replaceMap : String -> Text.Text -> (String -> Text.Text) -> String -> Text.Text
replaceMap s t f =
  String.split s
    >> List.map (\token -> if String.isEmpty token then Text.empty else f token)
    >> Text.join t


linkQualified : List String -> String -> Text.Text
linkQualified modules token =
  case List.reverse (String.split "." token) of
    name :: rest ->
      let
        qualifiers = List.reverse rest
      in
        if List.member (String.join "." qualifiers) modules
        then
          Text.link
            (String.join "-" qualifiers ++ "#" ++ name)
            (Text.fromString name)
        else
          Text.fromString name


dropQualifiers : String -> String
dropQualifiers =
  Regex.replace Regex.All qualifiers (always "")


qualifiers : Regex.Regex
qualifiers =
  Regex.regex "[A-Za-z1-9_]*\\."


-- VIEW HELPERS

green : String -> Text.Text
green str =
  Text.color (C.green) (Text.fromString str)


-- SPLITTING TYPES

type alias SplitState =
  { parenDepth : Int
  , bracketDepth : Int
  , currentChunk : String
  , chunks : List String
  }


updateDepths : Char -> SplitState -> SplitState
updateDepths char state =
  case char of
    '(' ->
        { state | parenDepth <- state.parenDepth + 1 }

    ')' ->
        { state | parenDepth <- state.parenDepth - 1 }

    '{' ->
        { state | bracketDepth <- state.bracketDepth + 1 }

    '}' ->
        { state | bracketDepth <- state.bracketDepth - 1 }

    _ ->
        state


-- SPLIT FUNCTION TYPES

splitArgs : String -> List String
splitArgs tipe =
  let
    formattedType =
      String.join "$" (String.split "->" tipe)

    state =
      String.foldl splitArgsHelp (SplitState 0 0 "" []) formattedType
  in
    List.reverse (String.reverse state.currentChunk :: state.chunks)
      |> List.map (String.split "$" >> String.join "->")


splitArgsHelp : Char -> SplitState -> SplitState
splitArgsHelp char startState =
  let
    state =
      updateDepths char startState
  in
    if char == '$' && state.parenDepth == 0 && state.bracketDepth == 0 then
        { state |
            currentChunk <- "",
            chunks <- String.reverse state.currentChunk :: state.chunks
        }
    else
        { state |
            currentChunk <- String.cons char state.currentChunk
        }


-- SPLIT RECORD TYPES

splitRecord : String -> List String
splitRecord tipe =
  let
    state =
      String.foldl splitRecordHelp (SplitState 0 0 "" []) tipe
  in
    List.reverse (String.reverse state.currentChunk :: state.chunks)


splitRecordHelp : Char -> SplitState -> SplitState
splitRecordHelp char startState =
  let
    state =
      updateDepths char startState
  in
    if state.bracketDepth == 0 then
        { state |
            currentChunk <- "}",
            chunks <- String.reverse state.currentChunk :: state.chunks
        }
    else if char == ',' && state.parenDepth == 0 && state.bracketDepth == 1 then
        { state |
            currentChunk <- ",",
            chunks <- String.reverse state.currentChunk :: state.chunks
        }
    else
        { state |
            currentChunk <- String.cons char state.currentChunk
        }
