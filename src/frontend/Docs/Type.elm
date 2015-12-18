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


toHtml : Name.Dictionary -> Context -> Type -> List Html
toHtml nameDict context tipe =
  let
    go ctx t =
      toHtml nameDict ctx t
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
        [ Name.toLink nameDict name ]

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
          maybeAddParens (Name.toLink nameDict name :: argsHtml)

    Tuple args ->
      List.map (go Other) args
        |> List.intersperse [text ", "]
        |> List.concat
        |> Code.addParens

    Record fields ext ->
        let
          fieldsHtml =
            List.map (fieldToHtml nameDict) fields
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


fieldToHtml : Name.Dictionary -> (String, Type) -> List Html
fieldToHtml nameDict (field, tipe) =
  text field :: space :: colon :: space :: toHtml nameDict Other tipe



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



-- SEARCH


containsQuery : String -> Type -> Bool
containsQuery query tipe =
  case tipe of
    Function args result ->
        let
          argsContainQuery =
            List.any (\b -> b) (List.map (\t -> containsQuery query t) args)
        in
          containsQuery query result || argsContainQuery

    Var name ->
        String.contains query name

    Apply {name} [] ->
        String.contains query name

    Apply {name} args ->
        let
          argsContainQuery =
            List.any (\b -> b) (List.map (\t -> containsQuery query t) args)
        in
          String.contains query name || argsContainQuery

    Tuple args ->
        List.any (\b -> b) (List.map (\t -> containsQuery query t) args)

    Record fields ext ->
        let
          inField (field, tipe) =
            String.contains query field || containsQuery query tipe

          inRecord =
            List.any (\b -> b) (List.map (\ft -> inField ft) fields)

          inExt =
            case ext of
              Nothing ->
                False

              Just extName ->
                String.contains query extName
        in
          inRecord || inExt


similarity : Type -> Type -> Int
similarity a b =
  let
    typeSimilarity = 0

    compareNames nameA nameB =
      if nameA == nameB then
        10
      else if String.contains nameA nameB then
        1
      -- else if nameA /= nameB then
      --   -10
      else
        0

  in
    case (a, b) of

      (Function argsA resultA, Function argsB resultB) ->
          if List.length argsA == List.length argsB then
            typeSimilarity
              + List.sum (List.map2 similarity argsA argsB)
              + similarity resultA resultB
          else
            typeSimilarity

      (Var nameA, Var nameB) ->
          typeSimilarity
            + compareNames nameA nameB

      (Apply canonicalA [], Apply canonicalB []) ->
          typeSimilarity
            + compareNames canonicalA.name canonicalB.name

      (Apply canonicalA argsA, Apply canonicalB argsB) ->
          if List.length argsA == List.length argsB then
            typeSimilarity
              + compareNames canonicalA.name canonicalB.name
              + List.sum (List.map2 similarity argsA argsB)
          else
            typeSimilarity

      (Tuple argsA, Tuple argsB) ->
          typeSimilarity
            + List.sum (List.map2 similarity argsA argsB)

      _ -> 0