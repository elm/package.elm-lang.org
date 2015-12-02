module Docs.Diff (diffPackage, PackageChanges) where

import Dict
import Effects as Fx exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import Set
import String

import Docs.Entry as Entry
import Docs.Name as Name
import Docs.Package as Docs
import Docs.Type as Type exposing (Type)



-- CHANGE TRACKERS


type alias Changes v =
    { added : Dict.Dict String v
    , changed : Dict.Dict String (v, v)
    , removed : Dict.Dict String v
    }


toChanges : (a -> a -> Maybe (a,a)) -> Dict.Dict comparable a -> Dict.Dict comparable a -> Changes a
toChanges combine old new =
  { added =
      Dict.diff new old
  , removed =
      Dict.diff old new
  , changed =
      intersectWith combine old new
  }


intersectWith
    : (a -> b -> Maybe c)
    -> Dict.Dict comparable a
    -> Dict.Dict comparable b
    -> Dict.Dict comparable c
intersectWith combine leftDict rightDict =
  List.foldl
    (intersectWithHelp combine rightDict)
    Dict.empty
    (Dict.toList leftDict)


intersectWithHelp
    : (a -> b -> Maybe c)
    -> Dict.Dict comparable b
    -> (comparable, a)
    -> Dict.Dict comparable c
    -> Dict.Dict comparable c
intersectWithHelp combine rightDict (key, a) combinedDict =
  case Dict.get key rightDict `Maybe.andThen` combine a of
    Nothing ->
      combinedDict

    Just c ->
      Dict.insert key c combinedDict


allTrue : List Bool -> Bool
allTrue list =
  case list of
    [] ->
      True

    b :: bs ->
      if b then
        allTrue bs

      else
        False



-- PACKAGE DIFFS


type alias PackageChanges =
    { added : List String
    , changed : Dict.Dict String (Changes (Entry.Model Type))
    , removed : List String
    }


diffPackage : Docs.Package Type -> Docs.Package Type -> PackageChanges
diffPackage old new =
  PackageChanges
    (Dict.keys (Dict.diff new old))
    (intersectWith maybeDiffModule old new)
    (Dict.keys (Dict.diff old new))


maybeDiffModule : Docs.Module Type -> Docs.Module Type -> Maybe (Changes (Entry.Model Type))
maybeDiffModule old new =
  let
    ({added,changed,removed} as changes) =
      diffModule old new
  in
    if Dict.isEmpty added && Dict.isEmpty changed && Dict.isEmpty removed then
      Nothing

    else
      Just changes



-- MODULE DIFFS


diffModule : Docs.Module Type -> Docs.Module Type -> Changes (Entry.Model Type)
diffModule old new =
  let
    ignoreHome =
      old.hasNonCanonicalTypes || new.hasNonCanonicalTypes

    toDiffPair oldEntry newEntry =
      if isEquivalent ignoreHome oldEntry newEntry then
        Just (oldEntry, newEntry)

      else
        Nothing
  in
    toChanges toDiffPair old.entries new.entries


isEquivalent : Bool -> Entry.Model Type -> Entry.Model Type -> Bool
isEquivalent ignoreHome oldEntry newEntry =
  case (oldEntry.info, newEntry.info) of
    (Entry.Value oldTipe oldFixity, Entry.Value newTipe newFixity) ->
      isEquivalentType ignoreHome ([], oldTipe) ([], newTipe)
      && oldFixity == newFixity

    (Entry.Union oldInfo, Entry.Union newInfo) ->
      isEquivalentUnion ignoreHome oldInfo newInfo

    (Entry.Alias old, Entry.Alias new) ->
      isEquivalentType ignoreHome (old.vars, old.tipe) (new.vars, new.tipe)

    (_, _) ->
      False


isEquivalentUnion : Bool -> Entry.UnionInfo Type -> Entry.UnionInfo Type -> Bool
isEquivalentUnion ignoreHome old new =
  if List.length old.tags /= List.length new.tags then
    False

  else
    let
      isEquivalentArgList oldTypes newTypes =
        List.length oldTypes == List.length newTypes
        &&
          allTrue (
            List.map2
              (isEquivalentType ignoreHome)
              (List.map ((,) old.vars) oldTypes)
              (List.map ((,) new.vars) newTypes)
          )
    in
      allTrue (List.map2 (==) (List.map .tag old.tags) (List.map .tag new.tags))
      &&
      allTrue (List.map2 isEquivalentArgList (List.map .args old.tags) (List.map .args new.tags))


isEquivalentType : Bool -> (List String, Type) -> (List String, Type) -> Bool
isEquivalentType ignoreHome (oldVars, oldType) (newVars, newType) =
  case diffType ignoreHome oldType newType of
    Nothing ->
        False

    Just renamings ->
        List.length oldVars == List.length newVars
        && isEquivalentRenaming (List.map2 (,) oldVars newVars ++ renamings)



-- TYPE DIFFS


diffType : Bool -> Type -> Type -> Maybe (List (String,String))
diffType ignoreHome oldType newType =
  case (oldType, newType) of
    (Type.Var oldName, Type.Var newName) ->
      Just [(oldName, newName)]

    (Type.Tuple oldArgs, Type.Tuple newArgs) ->
      diffTypeList ignoreHome oldArgs newArgs

    (Type.Apply oldName oldArgs, Type.Apply newName newArgs) ->
      if rawName ignoreHome oldName /= rawName ignoreHome newName then
        Nothing

      else
        diffTypeList ignoreHome oldArgs newArgs

    (Type.Function oldArgs oldResult, Type.Function newArgs newResult) ->
        Maybe.map2
          (++)
          (diffTypeList ignoreHome oldArgs newArgs)
          (diffType ignoreHome oldResult newResult)

    (Type.Record oldFields oldExt, Type.Record newFields newExt) ->
        case (oldExt, newExt) of
          (Nothing, Just _) ->
            Nothing

          (Just _, Nothing) ->
            Nothing

          (Nothing, Nothing) ->
            diffFields ignoreHome oldFields newFields

          (Just oldVar, Just newVar) ->
            Maybe.map
              ((::) (oldVar, newVar))
              (diffFields ignoreHome oldFields newFields)

    (_, _) ->
      Nothing


diffFields : Bool -> List (String, Type) -> List (String, Type) -> Maybe (List (String,String))
diffFields ignoreHome oldFields newFields =
  let
    (oldNames, oldTypes) =
      List.unzip (List.sortBy fst oldFields)

    (newNames, newTypes) =
      List.unzip (List.sortBy fst newFields)
  in
    if allTrue (List.map2 (==) oldNames newNames) then
      diffTypeList ignoreHome oldTypes newTypes

    else
      Nothing


rawName : Bool -> Name.Canonical -> String
rawName ignoreHome name =
  if ignoreHome then
    name.name

  else
    Name.nameToString name


diffTypeList : Bool -> List Type -> List Type -> Maybe (List (String, String))
diffTypeList ignoreHome oldTypes newTypes =
  case (oldTypes, newTypes) of
    (old :: olds, new :: news) ->
      Maybe.map2
        (++)
        (diffType ignoreHome old new)
        (diffTypeList ignoreHome olds news)

    ([], []) ->
      Just []

    (_, _) ->
      Nothing



-- TYPE VARIABLES DIFFS


isEquivalentRenaming : List (String, String) -> Bool
isEquivalentRenaming varPairs =
  let
    (renamings, acceptable) =
      List.unzip (Dict.values (List.foldr processPair Dict.empty varPairs))
  in
    allTrue acceptable
    && List.length renamings == Set.size (Set.fromList renamings)


processPair : (String, String) -> Dict.Dict String (String, Bool) -> Dict.Dict String (String, Bool)
processPair (old, new) dict =
  let
    trackUnique entry =
      case entry of
        Nothing ->
          Just (new, isSuperSet old new)

        Just (prevNew, isAcceptable) ->
          Just (new, isAcceptable && new == prevNew)

  in
    Dict.update old trackUnique dict


isSuperSet : String -> String -> Bool
isSuperSet old new =
  case (categorizeVar old, categorizeVar new) of
    (_, Var) ->
      True

    (Number, Comparable) ->
      True

    (CompAppend, Comparable) ->
      True

    (CompAppend, Appendable) ->
      True

    (oldCategory, newCategory) ->
      oldCategory == newCategory


type VarCategory
    = Comparable
    | Appendable
    | CompAppend
    | Number
    | Var


categorizeVar : String -> VarCategory
categorizeVar name =
  if String.startsWith "comparable" name then
    Comparable

  else if String.startsWith "appendable" name then
    Appendable

  else if String.startsWith "compappend" name then
    CompAppend

  else if String.startsWith "number" name then
    Number

  else
    Var
