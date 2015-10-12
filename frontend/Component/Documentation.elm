module Component.Documentation where

import Char
import Color
import Debug
import Dict
import Graphics.Element exposing (..)
import Json.Decode as Json exposing (..)
import Markdown
import Regex
import String
import Text

import ColorScheme as C


type alias DocDict =
    Dict.Dict String (Text.Text, Maybe (String, Int), String)


toDocDict : List String -> List (String, String) -> Documentation -> DocDict
toDocDict modules mentionedTypes docs =
  let toPairs view getAssocPrec entries =
          List.map (\entry -> (entry.name, (view entry, getAssocPrec entry, entry.comment))) entries

      ambiguousTypes =
          dropUniques (List.sort (List.map snd mentionedTypes))
  in
      Dict.fromList <|
        toPairs (viewAlias modules ambiguousTypes) (always Nothing) docs.aliases
        ++ toPairs (viewUnion modules ambiguousTypes) (always Nothing) docs.unions
        ++ toPairs (viewValue modules ambiguousTypes) .assocPrec docs.values


-- assumes the input list is sorted
dropUniques : List String -> List String
dropUniques list =
  case list of
    x :: y :: zs ->
      if x == y then x :: dropUniques zs else dropUniques (y :: zs)

    _ ->
      []


-- MODEL

type alias Documentation =
    { name : String
    , comment : String
    , aliases : List Alias
    , unions : List Union
    , values : List Value
    }


mentionedTypes : Documentation -> List (String, String)
mentionedTypes { name, aliases, unions, values } =
  let
    moduleName = name

    extractFromAlias { name, tipe } =
      (moduleName ++ ".", name) :: extractQualifiersAndType tipe

    extractFromUnion { name, cases } =
      (moduleName ++ ".", name) :: List.concatMap (List.concatMap extractQualifiersAndType << snd) cases

    extractFromValue { tipe } =
      extractQualifiersAndType tipe
  in
    List.concat <|
      List.map extractFromAlias aliases
      ++ List.map extractFromUnion unions
      ++ List.map extractFromValue values


extractQualifiersAndType : Type -> List (String, String)
extractQualifiersAndType tipe =
  let
    extract regexResult =
        case regexResult.submatches of
          [Just qs, _, Just t] ->
              (qs, t)

          _ ->
              Debug.crash "the regex qualifiersAndType should never produce other results"
  in
    List.map extract (Regex.find Regex.All qualifiersAndType tipe)


qualifiersAndType : Regex.Regex
qualifiersAndType =
  Regex.regex "(([A-Z][A-Za-z1-9_]*\\.)*)([A-Z][A-Za-z1-9_]*)"


documentation : Decoder Documentation
documentation =
  object5 Documentation
    ("name" := string)
    ("comment" := string)
    ("aliases" := list alias)
    ("types" := list union)
    ("values" := list value)


valueList : Decoder (String, List (String, String))
valueList =
  let
    allNames =
      object3 buildPairs
        ("aliases" := list ("name" := string))
        ("types" := list tipe)
        ("values" := list ("name" := string))

    -- build pairings
    buildPairs aliases types values =
      selfPair aliases ++ List.concat types ++ selfPair values

    selfPair names =
      List.map (\n -> (n, n)) names

    -- extract types
    tipe =
      object2 tagPairs ("name" := string) ("cases" := constructorList)

    constructorList =
      list (tuple2 always string Json.value)

    tagPairs name tags =
      List.map ((,) name) (name :: List.filter ((/=) name) tags)
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
    { defaults | sanitize = True }


viewEntry : Int -> String -> (Text.Text, Maybe (String, Int), String) -> Element
viewEntry innerWidth name (annotation, maybeAssocPrec, comment) =
  let
    rawAssocPrec =
      case maybeAssocPrec of
        Nothing ->
            empty

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

viewAlias : List String -> List String -> Alias -> Text.Text
viewAlias modules ambiguousTypes alias =
  Text.concat
    [ green "type alias "
    , Text.link ("#" ++ alias.name) (Text.bold (Text.fromString alias.name))
    , Text.fromString (String.concat (List.map ((++) " ") alias.args))
    , green " = "
    , case String.uncons alias.tipe of
        Just ('{', _) ->
          viewRecordType modules ambiguousTypes alias.tipe

        _ ->
          typeToText modules ambiguousTypes alias.tipe
    ]


-- VIEW UNIONS

viewUnion : List String -> List String -> Union -> Text.Text
viewUnion modules ambiguousTypes union =
  let
    seperators =
      green "\n    = "
      :: List.repeat (List.length union.cases - 1) (green "\n    | ")
  in
    Text.concat
      [ green "type "
      , Text.link ("#" ++ union.name) (Text.bold (Text.fromString union.name))
      , Text.fromString (String.concat (List.map ((++) " ") union.args))
      , Text.concat (List.map2 (++) seperators (List.map (viewCase modules ambiguousTypes) union.cases))
      ]


viewCase : List String -> List String -> (String, List Type) -> Text.Text
viewCase modules ambiguousTypes (tag, args) =
  List.map (viewArg modules ambiguousTypes) args
    |> (::) (Text.fromString tag)
    |> List.intersperse (Text.fromString " ")
    |> Text.concat


viewArg : List String -> List String -> String -> Text.Text
viewArg modules ambiguousTypes tipe =
  case String.uncons tipe of
    Just (c,_) ->
        if c == '(' || c == '{' || not (String.contains " " tipe) then
          typeToText modules ambiguousTypes tipe

        else
          typeToText modules ambiguousTypes ("(" ++ tipe ++ ")")


    Nothing ->
        Debug.crash "impossible to have an empty string as a type argument"


-- VIEW VALUES

viewValue : List String -> List String -> Value -> Text.Text
viewValue modules ambiguousTypes value =
  Text.concat
    [ Text.link ("#" ++ value.name) (Text.bold (viewVar value.name))
    , viewFunctionType modules ambiguousTypes value.tipe
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

viewRecordType : List String -> List String -> String -> Text.Text
viewRecordType modules ambiguousTypes tipe =
  splitRecord tipe
    |> List.map (Text.append (Text.fromString "\n    ") << typeToText modules ambiguousTypes)
    |> Text.concat


viewFunctionType : List String -> List String -> Type -> Text.Text
viewFunctionType modules ambiguousTypes tipe =
  if String.length (dropQualifiers tipe) < 80 then
    green " : " ++ typeToText modules ambiguousTypes tipe
  else
    let
      parts =
        splitArgs tipe

      seperators =
        green "\n    :  "
        :: List.repeat (List.length parts - 1) (green "\n    ->")
    in
      Text.concat (List.map2 (\sep -> Text.append sep << typeToText modules ambiguousTypes) seperators parts)


-- TYPE TO TEXT

typeToText : List String -> List String -> String -> Text.Text
typeToText modules ambiguousTypes =
  replaceMap " " (Text.fromString " ")
    <| replaceMap "," (Text.fromString ",")
    <| replaceMap "(" (Text.fromString "(")
    <| replaceMap ")" (Text.fromString ")")
    <| replaceMap "{" (Text.fromString "{")
    <| replaceMap "}" (Text.fromString "}")
    <| replaceMap "->" (green "->")
    <| replaceMap ":" (green ":")
    <| linkQualified modules ambiguousTypes


replaceMap : String -> Text.Text -> (String -> Text.Text) -> String -> Text.Text
replaceMap s t f =
  String.split s
    >> List.map (\token -> if String.isEmpty token then Text.empty else f token)
    >> Text.join t


linkQualified : List String -> List String -> String -> Text.Text
linkQualified modules ambiguousTypes token =
  let
    (qualifiers, name) =
      splitLast (String.split "." token)

    nameToShow =
      if List.member name ambiguousTypes then
          token
      else
          name
  in
    if List.member (String.join "." qualifiers) modules then

        Text.link (String.join "-" qualifiers ++ "#" ++ name) (Text.fromString nameToShow)

    else

        Text.fromString nameToShow


splitLast : List a -> (List a, a)
splitLast list =
  case list of
    [] ->
        Debug.crash "cannot call splitLast on an empty list"

    [x] ->
        ([], x)

    x :: rest ->
        let
          (xs, last) = splitLast rest
        in
          (x :: xs, last)


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
        { state | parenDepth = state.parenDepth + 1 }

    ')' ->
        { state | parenDepth = state.parenDepth - 1 }

    '{' ->
        { state | bracketDepth = state.bracketDepth + 1 }

    '}' ->
        { state | bracketDepth = state.bracketDepth - 1 }

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
            currentChunk = "",
            chunks = String.reverse state.currentChunk :: state.chunks
        }
    else
        { state |
            currentChunk = String.cons char state.currentChunk
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
            currentChunk = "}",
            chunks = String.reverse state.currentChunk :: state.chunks
        }
    else if char == ',' && state.parenDepth == 0 && state.bracketDepth == 1 then
        { state |
            currentChunk = ",",
            chunks = String.reverse state.currentChunk :: state.chunks
        }
    else
        { state |
            currentChunk = String.cons char state.currentChunk
        }
