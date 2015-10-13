module Docs.Entry where

import Effects as Fx exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import String

import Docs.Name as Name
import Docs.Type as Type exposing (Type)
import Utils.Code exposing (arrow, colon, equals, keyword, padded, space)
import Utils.Markdown as Markdown



-- MODEL


type alias Model =
    { name : String
    , info : Info
    , docs : String
    }


type Info
    = Value Type (Maybe Fixity)
    | Union
        { vars : List String
        , tags : List Tag
        }
    | Alias
        { vars : List String
        , tipe : Type
        }


type alias Tag =
    { tag : String
    , args : List Type
    }


type alias Fixity =
    { precedence : Int
    , associativity : String
    }



-- UPDATE


update : a -> Model -> (Model, Effects a)
update action model =
  (model, Fx.none)



-- VIEW


(=>) = (,)


view : Signal.Address a -> Model -> Html
view _ model =
  let
    annotation =
      case model.info of
        Value tipe _ ->
            valueAnnotation model.name tipe

        Union {vars,tags} ->
            unionAnnotation model.name vars tags

        Alias {vars,tipe} ->
            aliasAnnotation model.name vars tipe
  in
    div [ class "docs-entry" ]
      [ annotationBlock annotation
      , Markdown.block model.docs
      ]


annotationBlock : List (List Html) -> Html
annotationBlock bits =
  div [ class "docs-annotation" ]
    (List.concat (List.intersperse [text "\n"] bits))


nameToLink : String -> Html
nameToLink name =
  a [href ("#" ++ name)] [text name]



-- VALUE ANNOTATIONS


valueAnnotation : String -> Type -> List (List Html)
valueAnnotation name tipe =
  case tipe of
    Type.Function args result ->
        if Type.length Type.Other tipe > 120 then
            [ nameToLink name ] :: longFunctionAnnotation args result

        else
            [ nameToLink name :: padded colon ++ Type.toHtml Type.Other tipe ]

    _ ->
        [ nameToLink name :: padded colon ++ Type.toHtml Type.Other tipe ]


longFunctionAnnotation : List Type -> Type -> List (List Html)
longFunctionAnnotation args result =
  let
    tipeHtml =
      List.map (Type.toHtml Type.Func) (args ++ [result])

    starters =
      [ text "    ", colon, text "  " ]
      ::
      List.repeat (List.length args) [ text "    ", arrow, space ]
  in
    List.map2 (++) starters tipeHtml



-- UNION ANNOTATIONS


unionAnnotation : String -> List String -> List Tag -> List (List Html)
unionAnnotation name vars tags =
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
        (List.map viewTag tags)
  in
    nameLine :: tagLines


viewTag : Tag -> List Html
viewTag {tag,args} =
  text tag :: List.concatMap ((::) space) (List.map (Type.toHtml Type.App) args)



-- ALIAS ANNOTATIONS


aliasAnnotation : String -> List String -> Type -> List (List Html)
aliasAnnotation name vars tipe =
  let
    nameLine =
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

                    Just extTipe ->
                      ( [ text "    { " :: Type.toHtml Type.Other extTipe ++ [text " |"] ]
                      , text "      | " :: List.repeat (List.length fields) (text "      , ")
                      )
            in
              firstLine
              ++ List.map2 (::) starters (List.map Type.fieldToHtml fields)
              ++ [[text "    }"]]

        _ ->
            [ Type.toHtml Type.Other tipe ]
  in
    nameLine :: typeLines

