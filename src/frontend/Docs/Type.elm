module Docs.Type where

import Char
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


-- TODO: avoid the duplication which only exists because of the links with basePath
toHtmlWithBasePath : String -> Name.Dictionary -> Context -> Type -> List Html
toHtmlWithBasePath basePath nameDict context tipe =
  let
    go ctx t =
      toHtmlWithBasePath basePath nameDict ctx t
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
        [ Name.toBaseLink basePath nameDict name ]

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
          maybeAddParens (Name.toBaseLink basePath nameDict name :: argsHtml)

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


distance : Type -> Type -> Int
distance a b =
  let
    defaultPenalty = 10
  in
    case (a, b) of

      (Function argsA resultA, Function argsB resultB) ->
          if List.length argsA == List.length argsB then
            List.sum (List.map2 distance argsA argsB)
              + distance resultA resultB
          else
            defaultPenalty * (abs (List.length argsA - List.length argsB))

      (Var nameA, Var nameB) ->
          compareNamesWithPenalty defaultPenalty nameA nameB

      (Apply canonicalA [], Apply canonicalB []) ->
          compareNamesWithPenalty 2 canonicalA.home canonicalB.home
          + compareNamesWithPenalty 2 canonicalA.name canonicalB.name

      (Apply canonicalA argsA, Apply canonicalB argsB) ->
          if List.length argsA == List.length argsB then
            compareNamesWithPenalty 2 canonicalA.home canonicalB.home
              + compareNamesWithPenalty 2 canonicalA.name canonicalB.name
              + List.sum (List.map2 distance argsA argsB)
          else
            defaultPenalty * (abs (List.length argsA - List.length argsB))

      (Tuple argsA, Tuple argsB) ->
          List.sum (List.map2 distance argsA argsB)

      _ -> 100


compareNamesWithPenalty : Int -> String -> String -> Int
compareNamesWithPenalty penalty nameA nameB =
  if nameA == nameB then
    0
  else if String.contains nameA nameB then
    1
  else
    penalty



type alias Mapping = Dict.Dict String String


defaultMapping : Mapping
defaultMapping =
  Dict.empty
    |> Dict.insert "number" "number"
    |> Dict.insert "comparable" "comparable"
    |> Dict.insert "appendable" "appendable"


nextMappingValue : Mapping -> String
nextMappingValue mapping =
  let
    base = (Dict.size mapping) - (Dict.size defaultMapping)
    code = (base % 26) + (Char.toCode 'a')
    string = String.fromChar (Char.fromCode code)
    times = (base // 26) + 1
  in
    String.repeat times string


updateMapping : Type -> Mapping -> Mapping
updateMapping tipe mapping =
  let
    updateMappingFor name =
      if Dict.member name mapping then
        mapping
      else
        Dict.insert
          name
          (nextMappingValue mapping)
          mapping
  in
    case tipe of
      Function args result ->
        List.foldl updateMapping mapping (List.append args [result])

      Var name -> updateMappingFor name

      Apply name args ->
          List.foldl updateMapping mapping args

      Tuple args ->
          List.foldl updateMapping mapping args

      Record fields ext ->
          List.foldl updateMapping mapping (List.map (\ (_, t) -> t) fields)


normalize : Type -> Type
normalize tipe =
  normalizeWithMapping (updateMapping tipe defaultMapping) tipe


normalizeWithMapping : Mapping -> Type -> Type
normalizeWithMapping mapping tipe =
  let
    normalize' = normalizeWithMapping mapping
  in
    case tipe of
      Function args result ->
          Function
            (List.map normalize' args)
            (normalize' result)

      Var name ->
          let
            name' =
              case Dict.get name mapping of
                Just n -> n
                Nothing -> name
          in
            Var name'

      Apply name args ->
          Apply name (List.map normalize' args)

      Tuple args ->
          Tuple (List.map normalize' args)

      Record fields ext ->
          Record (List.map (\ (k, v) -> (k, normalize' v)) fields) ext
