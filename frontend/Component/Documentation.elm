module Component.Documentation where

import Basics (..)
import Debug
import Dict
import Graphics.Element (..)
import JavaScript (..)
import JavaScript as JS
import List
import List ((::))
import Maybe (Maybe(Just,Nothing))
import Result (Result(Ok,Err))
import String
import Text

import ColorScheme as C


toDocDict : Documentation -> Dict.Dict String Element
toDocDict docs =
  let toPairs view entries =
          List.map (\entry -> (entry.name, view entry)) entries
  in
      Dict.fromList <|
        toPairs viewAlias docs.aliases
        ++ toPairs viewUnion docs.unions
        ++ toPairs viewValue docs.values


-- MODEL

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
    , assocPrec : Maybe (String,Int)
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
    | Record [(String, Type)] (Maybe Type)


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


-- VIEW

viewAlias : Alias -> Element
viewAlias alias =
  let annotation = Text.concat [ p1, p2, p3, p4, p5 ]
      
      p1 = blueString "type alias "
      p2 = Text.bold (Text.fromString alias.name)
      p3 = spacePrefix (List.map Text.fromString alias.args)
      p4 = equals
      p5 = viewType alias.tipe
  in
      Text.leftAligned (Text.monospace annotation)


viewUnion : Union -> Element
viewUnion union =
  let seperators =
        "=" :: List.repeat (List.length union.cases - 1) "|"
                
      annotation =
        Text.concat
        [ blueString "type "
        , Text.bold (Text.fromString union.name)
        , spacePrefix (List.map Text.fromString union.args)
        , Text.concat (List.map2 viewCase seperators union.cases)
        ]
  in
      Text.leftAligned (Text.monospace annotation)


viewCase : String -> (String, [Type]) -> Text.Text
viewCase sep (tag, args) =
  Text.fromString "\n    " ++ blueString sep
  ++ spacePrefix (Text.fromString tag :: List.map (viewTypeHelp ADT) args)


viewValue : Value -> Element
viewValue value =
  let annotation =
        Text.concat
        [ Text.bold (Text.fromString value.name)
        , colon
        , viewType value.tipe
        ]
  in
      Text.leftAligned (Text.monospace annotation)



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


spacePrefix : [Text.Text] -> Text.Text
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