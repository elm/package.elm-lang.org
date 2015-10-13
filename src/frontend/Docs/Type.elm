module Docs.Type where

import Effects as Fx exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import String

import Docs.Name as Name
import Utils.Code as Code exposing (arrow, colon, padded, space)



-- MODEL


type Type
    = Function (List Type) Type
    | Var String
    | Apply Name.Canonical (List Type)
    | Record (List (String, Type)) (Maybe Type)


type alias Tag =
    { tag : String
    , args : List Type
    }



-- TYPE TO FLAT HTML


type Context = Func | App | Other



toHtml : Context -> Type -> List Html
toHtml context tipe =
  case tipe of
    Function args result ->
        let
          maybeAddParens =
            case context of
              Func -> Code.addParens
              App -> Code.addParens
              Other -> identity

          argsHtml =
            List.concatMap (\arg -> toHtml Func arg ++ padded arrow) args
        in
          maybeAddParens (argsHtml ++ toHtml Func result)

    Var name ->
        [ text name ]

    Apply name args ->
        let
          maybeAddParens =
            case context of
              Func -> identity
              App -> Code.addParens
              Other -> identity

          argsHtml =
            List.concatMap (\arg -> space :: toHtml App arg) args
        in
          maybeAddParens (Name.toLink name :: argsHtml)

    Record fields ext ->
        let
          fieldsHtml =
            List.map fieldToHtml fields
              |> List.intersperse [text ", "]
              |> List.concat

          recordInsides =
            case ext of
              Nothing ->
                fieldsHtml

              Just tipe ->
                toHtml Other tipe ++ text " | " :: fieldsHtml
        in
          text "{ " :: recordInsides ++ [text " }"]


fieldToHtml : (String, Type) -> List Html
fieldToHtml (field, tipe) =
  text field :: space :: colon :: space :: toHtml Other tipe



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
            List.map (length Func) args

          arrows =
            4 * List.length args
        in
          parens + List.sum argLengths + arrows + length Func result

    Var name ->
        String.length name

    Apply name args ->
        let
          parens =
            case context of
              Func -> 0
              App -> 2
              Other -> 0

          nameLength =
            String.length name.home + 1 + String.length name.name

          argsLength =
            List.sum (List.map (\t -> 1 + length App t) args)
        in
          nameLength + argsLength + parens

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

              Just tipe ->
                2 + length Other tipe
        in
          recordLength + extLength


