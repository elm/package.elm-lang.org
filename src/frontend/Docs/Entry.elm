module Docs.Entry exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Regex
import String

import Docs.Name as Name
import Docs.Type as Type exposing (Type)
import Utils.Code exposing (arrow, colon, equals, keyword, padded, space)
import Utils.Markdown as Markdown



-- MODEL


type alias Model tipe =
    { name : String
    , info : Info tipe
    , docs : String
    }


type Info tipe
    = Value tipe (Maybe Fixity)
    | Union
        { vars : List String
        , tags : List (Tag tipe)
        }
    | Alias
        { vars : List String
        , tipe : tipe
        }


type alias Tag tipe =
    { tag : String
    , args : List tipe
    }


type alias Fixity =
    { precedence : Int
    , associativity : String
    }



-- UPDATE


update : msg -> Model tipe -> (Model tipe, Cmd msg)
update msg model =
  (model, Cmd.none)



-- MAP


map : (a -> b) -> Model a -> Model b
map func model =
  let
    newInfo =
      case model.info of
        Value tipe fixity ->
          Value (func tipe) fixity

        Union {vars,tags} ->
          Union { vars = vars, tags = List.map (tagMap func) tags }

        Alias {vars,tipe} ->
          Alias { vars = vars, tipe = func tipe }
  in
    { model | info = newInfo }


tagMap : (a -> b) -> Tag a -> Tag b
tagMap func tag =
  { tag | args = List.map func tag.args }



-- STRING VIEW


stringView : Model String -> Html msg
stringView model =
  let
    annotation =
      case model.info of
        Value tipe _ ->
            [ nameToLink model.name :: padded colon ++ [text tipe] ]

        Union {vars,tags} ->
            unionAnnotation (\t -> [text t]) model.name vars tags

        Alias {vars,tipe} ->
            [ aliasNameLine model.name vars
            , [ text "    ", text tipe ]
            ]
  in
    div [ class "docs-entry", id model.name ]
      [ annotationBlock annotation
      , div [class "docs-comment"] [Markdown.block model.docs]
      ]



-- TYPE VIEW


(=>) = (,)


typeView : Name.Dictionary -> Model Type -> Html msg
typeView nameDict model =
  let
    annotation =
      case model.info of
        Value tipe _ ->
            valueAnnotation nameDict model.name tipe

        Union {vars,tags} ->
            unionAnnotation (Type.toHtml nameDict Type.App) model.name vars tags

        Alias {vars,tipe} ->
            aliasAnnotation nameDict model.name vars tipe
  in
    div [ class "docs-entry", id model.name ]
      [ annotationBlock annotation
      , div [class "docs-comment"] [Markdown.block model.docs]
      ]


annotationBlock : List (List (Html msg)) -> Html msg
annotationBlock bits =
  div [ class "docs-annotation" ]
    (List.concat (List.intersperse [text "\n"] bits))


nameToLink : String -> Html msg
nameToLink name =
  let
    humanName =
      if Regex.contains operator name then
        "(" ++ name ++ ")"

      else
        name
  in
    a [style ["font-weight" => "bold"], href ("#" ++ name)] [text humanName]


operator : Regex.Regex
operator =
  Regex.regex "^[^a-zA-Z0-9]+$"



-- VALUE ANNOTATIONS


valueAnnotation : Name.Dictionary -> String -> Type -> List (List (Html msg))
valueAnnotation nameDict name tipe =
  case tipe of
    Type.Function args result ->
        if String.length name + 3 + Type.length Type.Other tipe > 64 then
            [ nameToLink name ] :: longFunctionAnnotation nameDict args result

        else
            [ nameToLink name :: padded colon ++ Type.toHtml nameDict Type.Other tipe ]

    _ ->
        [ nameToLink name :: padded colon ++ Type.toHtml nameDict Type.Other tipe ]


longFunctionAnnotation : Name.Dictionary -> List Type -> Type -> List (List (Html msg))
longFunctionAnnotation nameDict args result =
  let
    tipeHtml =
      List.map (Type.toHtml nameDict Type.Func) (args ++ [result])

    starters =
      [ text "    ", colon, text "  " ]
      ::
      List.repeat (List.length args) [ text "    ", arrow, space ]
  in
    List.map2 (++) starters tipeHtml



-- UNION ANNOTATIONS


unionAnnotation : (tipe -> List (Html msg)) -> String -> List String -> List (Tag tipe) -> List (List (Html msg))
unionAnnotation tipeToHtml name vars tags =
  let
    nameLine =
      [ keyword "type"
      , space
      , nameToLink name
      , text (String.concat (List.map ((++) " ") vars))
      ]

    tagLines =
      List.map2 (::)
        (text "    = " :: List.repeat (List.length tags - 1) (text "    | "))
        (List.map (viewTag tipeToHtml) tags)
  in
    nameLine :: tagLines


viewTag : (tipe -> List (Html msg)) -> Tag tipe -> List (Html msg)
viewTag tipeToHtml {tag,args} =
  text tag :: List.concatMap ((::) space) (List.map tipeToHtml args)



-- ALIAS ANNOTATIONS


aliasAnnotation : Name.Dictionary -> String -> List String -> Type -> List (List (Html msg))
aliasAnnotation nameDict name vars tipe =
  let
    typeLines =
      case tipe of
        Type.Record fields ext ->
            let
              (firstLine, starters) =
                  case ext of
                    Nothing ->
                      ( []
                      , text "    { " :: List.repeat (List.length fields) (text "    , ")
                      )

                    Just extName ->
                      ( [ [ text "    { ", text extName, text " |" ] ]
                      , text "      | " :: List.repeat (List.length fields) (text "      , ")
                      )
            in
              firstLine
              ++ List.map2 (::) starters (List.map (Type.fieldToHtml nameDict) fields)
              ++ [[text "    }"]]

        _ ->
            [ text "    " :: Type.toHtml nameDict Type.Other tipe ]
  in
    aliasNameLine name vars :: typeLines


aliasNameLine : String -> List String -> List (Html msg)
aliasNameLine name vars =
  [ keyword "type"
  , space
  , keyword "alias"
  , space
  , nameToLink name
  , text (String.concat (List.map ((++) " ") vars))
  , space
  , equals
  , space
  ]