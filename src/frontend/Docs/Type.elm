module Docs.Type where

import Dict
import Effects as Fx exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import Set
import String

import Docs.Name as Name
import Utils.Code as Code exposing (arrow, colon, padded, space)



-- MODEL


type Type
    = Function (List Type) Type
    | Var String
    | Apply Name.Canonical (List Type)
    | Tuple (List Type)
    | Record (List (String, Type)) (Maybe String)


type alias Tag =
    { tag : String
    , args : List Type
    }



-- TYPE TO FLAT HTML


type Context = Func | App | Other


toHtml : Name.Dictionary -> Name.Context -> Context -> Type -> List Html
toHtml nameDict docsContext context tipe =
  let
    go ctx t =
      toHtml nameDict docsContext ctx t

  in
  case tipe of
    Function args result ->
        let
          maybeAddParens =
            case context of
              Func -> Code.addParens
              App -> Code.addParens
              Other -> identity

          argsHtml =
            List.concatMap (\arg -> go Func arg ++ padded arrow) args
        in
          maybeAddParens (argsHtml ++ go Func result)

    Var name ->
        [ text name ]

    Apply name [] ->
        [ Name.toLink docsContext name ]

    Apply name args ->
        let
          maybeAddParens =
            case context of
              Func -> identity
              App -> Code.addParens
              Other -> identity

          argsHtml =
            List.concatMap (\arg -> space :: go App arg) args
        in
          maybeAddParens (Name.toLink docsContext name :: argsHtml)

    Tuple args ->
      List.map (go Other) args
        |> List.intersperse [text ", "]
        |> List.concat
        |> Code.addParens

    Record fields ext ->
        let
          fieldsHtml =
            List.map (fieldToHtml nameDict docsContext) fields
              |> List.intersperse [text ", "]
              |> List.concat

          recordInsides =
            case ext of
              Nothing ->
                fieldsHtml

              Just extName ->
                text extName :: text " | " :: fieldsHtml
        in
          text "{ " :: recordInsides ++ [text " }"]


fieldToHtml : Name.Dictionary -> Name.Context -> (String, Type) -> List Html
fieldToHtml nameDict docsContext (field, tipe) =
  text field :: space :: colon :: space :: toHtml nameDict docsContext Other tipe



-- TYPE LENGTH


length : Context -> Type -> Int
length context tipe =
  case tipe of
    Function args result ->
        let
          parens =
            case context of
              Func -> 2
              App -> 2
              Other -> 0

          argLengths =
            List.map (\t -> 4 + length Func t) args
        in
          parens + List.sum argLengths + length Func result

    Var name ->
        String.length name

    Apply {name} [] ->
        String.length name

    Apply {name} args ->
        let
          parens =
            case context of
              Func -> 0
              App -> 2
              Other -> 0

          argsLength =
            List.sum (List.map (\t -> 1 + length App t) args)
        in
          parens + String.length name + argsLength

    Tuple args ->
        List.sum (List.map (\t -> 2 + length Other t) args)

    Record fields ext ->
        let
          fieldLength (field, tipe) =
            String.length field + 3 + length Other tipe

          recordLength =
            2 + List.sum (List.map (\ft -> 2 + fieldLength ft) fields)

          extLength =
            case ext of
              Nothing ->
                0

              Just extName ->
                2 + String.length extName
        in
          recordLength + extLength
