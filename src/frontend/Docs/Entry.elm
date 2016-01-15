module Docs.Entry where

import Effects as Fx exposing (Effects)
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


update : a -> Model tipe -> (Model tipe, Effects a)
update action model =
  (model, Fx.none)



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



-- FILTER


nameSimilarity : String -> Model Type -> Int
nameSimilarity query model =
  case model.info of
    Value tipe _ ->
      if query == model.name then
        10
      else if String.contains query model.name then
        1
      else
        0

    _ ->
      0


typeSimilarity : Type -> Model Type -> Int
typeSimilarity queryType model =
  case model.info of
    Value tipe _ ->
      Type.similarity queryType (Type.normalize tipe)

    _ ->
      0



-- STRING VIEW


stringView : Model String -> Html
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


-- TODO: DRY this up
typeViewAnnotation : Name.Canonical -> Name.Dictionary -> Model Type -> Html
typeViewAnnotation canonical nameDict model =
  let
    annotation =
      case model.info of
        Value tipe _ ->
            valueAnnotationCanonical canonical nameDict model.name tipe

        Union {vars,tags} ->
            unionAnnotation (Type.toHtml nameDict Type.App) model.name vars tags

        Alias {vars,tipe} ->
            aliasAnnotation nameDict model.name vars tipe
  in
    div [ class "docs-entry" ]
      [ annotationBlock annotation ]


typeView : Name.Dictionary -> Model Type -> Html
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


annotationBlock : List (List Html) -> Html
annotationBlock bits =
  div [ class "docs-annotation" ]
    (List.concat (List.intersperse [text "\n"] bits))


nameToLink : String -> Html
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

-- TODO: DRY this up
valueAnnotationCanonical : Name.Canonical -> Name.Dictionary -> String -> Type -> List (List Html)
valueAnnotationCanonical canonical nameDict name tipe =
  case tipe of
    Type.Function args result ->
        if String.length name + 3 + Type.length Type.Other tipe > 64 then
            [ Name.toLink nameDict canonical ] :: longFunctionAnnotation nameDict args result

        else
            [ (Name.toLink nameDict canonical) :: padded colon ++ Type.toHtml nameDict Type.Other tipe ]

    _ ->
        [ Name.toLink nameDict canonical :: padded colon ++ Type.toHtml nameDict Type.Other tipe ]


valueAnnotation : Name.Dictionary -> String -> Type -> List (List Html)
valueAnnotation nameDict name tipe =
  case tipe of
    Type.Function args result ->
        if String.length name + 3 + Type.length Type.Other tipe > 64 then
            [ nameToLink name ] :: longFunctionAnnotation nameDict args result

        else
            [ nameToLink name :: padded colon ++ Type.toHtml nameDict Type.Other tipe ]

    _ ->
        [ nameToLink name :: padded colon ++ Type.toHtml nameDict Type.Other tipe ]


longFunctionAnnotation : Name.Dictionary -> List Type -> Type -> List (List Html)
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


unionAnnotation : (tipe -> List Html) -> String -> List String -> List (Tag tipe) -> List (List Html)
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


viewTag : (tipe -> List Html) -> Tag tipe -> List Html
viewTag tipeToHtml {tag,args} =
  text tag :: List.concatMap ((::) space) (List.map tipeToHtml args)



-- ALIAS ANNOTATIONS


aliasAnnotation : Name.Dictionary -> String -> List String -> Type -> List (List Html)
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


aliasNameLine : String -> List String -> List Html
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