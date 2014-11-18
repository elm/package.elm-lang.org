module Component.Documentation where

import Char
import Color
import Dict
import Graphics.Element (..)
import Json.Decode (..)
import List
import List ((::))
import Markdown
import String
import Text

import ColorScheme as C


type alias DocDict =
    Dict.Dict String (Text.Text, Maybe (String, Int), String)


toDocDict : Documentation -> DocDict
toDocDict docs =
  let toPairs view getAssocPrec entries =
          List.map (\entry -> (entry.name, (view entry, getAssocPrec entry, entry.comment))) entries
  in
      Dict.fromList <|
        toPairs viewAlias (always Nothing) docs.aliases
        ++ toPairs viewUnion (always Nothing) docs.unions
        ++ toPairs viewValue .assocPrec docs.values


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
      ("type" := tipe)


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
      ("cases" := list (tuple2 (,) string (list tipe)))


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
      ("type" := tipe)
      assocPrec

assocPrec : Decoder (Maybe (String, Int))
assocPrec =
  maybe <|
    object2 (,)
      ("associativity" := string)
      ("precedence" := int)


type Type
    = Lambda Type Type
    | Var String
    | Type String
    | App Type (List Type)
    | Record (List (String, Type)) (Maybe Type)


tipe : Decoder Type
tipe =
    ("tag" := string) `andThen` specificType


specificType : String -> Decoder Type
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


-- VIEW

viewEntry : Int -> (Text.Text, Maybe (String, Int), String) -> Element
viewEntry innerWidth (annotation, maybeAssocPrec, comment) =
  let rawAssocPrec =
        case maybeAssocPrec of
          Nothing -> empty
          Just (assoc, prec) ->
            assoc ++ "-associative / precedence " ++ toString prec
              |> Text.fromString
              |> Text.height 12
              |> Text.rightAligned

      assocPrecWidth =
        widthOf rawAssocPrec + 20

      assocPrec =
        container assocPrecWidth (min annotationHeight 24) middle rawAssocPrec

      annotationText =
        Text.leftAligned (Text.monospace annotation)
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
                , width (innerWidth - 40) (Markdown.toElement comment)
                ]

      annotationBar =
        color C.lightGrey <|
        flow right
        [ spacer annotationPadding annotationHeight
        , container annotationWidth annotationHeight midLeft annotationText
        , assocPrec
        ]
  in
      flow down
      [ color C.mediumGrey (spacer innerWidth 1)
      , annotationBar
      , commentElement
      , spacer innerWidth 20
      ]


viewAlias : Alias -> Text.Text
viewAlias alias =
    Text.concat
    [ blueString "type alias "
    , Text.bold (Text.fromString alias.name)
    , spacePrefix (List.map Text.fromString alias.args)
    , equals
    , viewType alias.tipe
    ]


viewUnion : Union -> Text.Text
viewUnion union =
  let seperators =
        "=" :: List.repeat (List.length union.cases - 1) "|"
  in
      Text.concat
      [ blueString "type "
      , Text.bold (Text.fromString union.name)
      , spacePrefix (List.map Text.fromString union.args)
      , Text.concat (List.map2 viewCase seperators union.cases)
      ]


viewCase : String -> (String, List Type) -> Text.Text
viewCase sep (tag, args) =
  Text.fromString "\n    " ++ blueString sep
  ++ spacePrefix (Text.fromString tag :: List.map (viewTypeHelp ADT) args)


viewValue : Value -> Text.Text
viewValue value =
    Text.concat
    [ Text.bold (viewVar value.name)
    , colon
    , viewType value.tipe
    ]


viewVar : String -> Text.Text
viewVar str =
  let txt = Text.fromString str
  in
      case String.uncons str of
        Nothing -> txt
        Just (c, _) ->
          if isVarChar c then txt else parens txt


isVarChar : Char -> Bool
isVarChar c =
    Char.isLower c || Char.isUpper c || Char.isDigit c || c == '_' || c == '\''



type Context = None | ADT | Function


viewType : Type -> Text.Text
viewType tipe =
  viewTypeHelp None tipe


viewTypeHelp : Context -> Type -> Text.Text
viewTypeHelp context tipe =
  case tipe of
    Lambda t1 t2 ->
        let txt = viewTypeHelp Function t1 ++ arrow ++ viewType t2
        in
            case context of
              None -> txt
              _ -> parens txt

    Var name ->
        Text.fromString name

    Type name ->
        Text.fromString name

    App (Type name) args ->
      if  | name == "_List" && List.length args == 1 ->
              sandwich "[" "]" (viewType (List.head args))

          | isTuple name ->
              List.map viewType args
                  |> List.intersperse (Text.fromString ", ")
                  |> Text.concat
                  |> parens

          | otherwise ->
              let txt = Text.fromString name ++ spacePrefix (List.map (viewTypeHelp ADT) args)
              in
                  case (context, args) of
                    (ADT, _ :: _) -> parens txt
                    _ -> txt

    Record fields extension ->
        let viewField (key, value) =
                Text.fromString key ++ colon ++ viewType value

            viewExtension maybeType =
                case maybeType of
                  Nothing -> Text.fromString ""
                  Just t -> viewType t ++ Text.fromString " | "
        in
            sandwich "{ " " }" <|
            Text.concat
            [ viewExtension extension
            , List.map viewField fields
                |> Text.join (Text.fromString ", ")
            ]


isTuple : String -> Bool
isTuple str =
  case String.toInt (String.dropLeft 6 str) of
    Err _ -> False
    Ok _ -> String.left 6 str == "_Tuple"


parens : Text.Text -> Text.Text
parens txt =
  sandwich "(" ")" txt


spacePrefix : List Text.Text -> Text.Text
spacePrefix txts =
  Text.concat (List.map (\txt -> space ++ txt) txts)


space : Text.Text
space =
  Text.fromString " "


equals : Text.Text
equals =
  pad (blueString "=")


bar : Text.Text
bar =
  pad (blueString "|")


arrow : Text.Text
arrow =
  pad (blueString "->")


colon : Text.Text
colon =
  pad (blueString ":")


blueString : String -> Text.Text
blueString str =
  Text.color (C.blue) (Text.fromString str)


pad : Text.Text -> Text.Text
pad txt =
  sandwich " " " " txt


sandwich : String -> String -> Text.Text -> Text.Text
sandwich start stop txt =
    Text.fromString start ++ txt ++ Text.fromString stop