module Session exposing
  ( Data
  , empty
  , Msg
  , update
  , load
  , Progress(..)
  )


import Dict
import Elm.Docs as Docs
import Http
import Json.Decode as Decode
import Page.Problem as Problem
import Release
import Route
import Session.Resource as Resource
import Version



-- SESSION DATA


type Data = Data Info


type alias Info =
  { releases : Dict.Dict String (Remote (List Release.Release))
  , readmes : Dict.Dict String (Remote String)
  , docs : Dict.Dict String (Remote (List Docs.Module))
  }


empty : Data
empty =
  Data (Info Dict.empty Dict.empty Dict.empty)



-- REMOTE


type alias Remote a =
  Result Resource.Error (Maybe a)


loading : Remote a
loading =
  Ok Nothing



-- UPDATE


type Msg
  = Load Resource.Result


update : Msg -> Data -> Data
update (Load resourceResult) (Data info) =
  Data <|
    case resourceResult of
      Resource.Releases user project result ->
        { info | releases = insert result info.releases (toPkgKey user project) (Resource.BadReleases user project) }

      Resource.Readme user project vsn result ->
        { info | readmes = insert result info.readmes (toVsnKey user project vsn) (Resource.BadReadme user project vsn) }

      Resource.Docs user project vsn result ->
        { info | docs = insert result info.docs (toVsnKey user project vsn) (Resource.BadDocs user project vsn) }


insert : Result Http.Error a -> Dict.Dict String (Remote a) -> String -> (Http.Error -> Resource.Error) -> Dict.Dict String (Remote a)
insert result dict key toError =
  case result of
    Err err ->
      Dict.insert key (Err (toError err)) dict

    Ok a ->
      Dict.insert key (Ok (Just a)) dict



-- PEEK


getLatestVersion : Data -> String -> String -> Maybe Version.Version
getLatestVersion (Data info) user project =
  case Dict.get (toPkgKey user project) info.releases of
    Just (Ok releases) ->
      Maybe.andThen Release.getLatestVersion releases

    _ ->
      Nothing



-- LOAD


type Progress
  = Loading Data (Maybe String) (Maybe (List Docs.Module)) (Cmd Msg)
  | Done String (List Docs.Module)
  | Problem Problem.Suggestion


load : String -> String -> Route.Version -> Maybe String -> Data -> Progress
load user project vsn maybeModule (Data info0) =
  let
    (info1, cmd1, remoteReleases) = loadReleases user project info0
    (info2, cmd2, remoteReadme) = loadReadme user project vsn info1
    (info3, cmd3, remoteDocs) = loadDocs user project vsn info2
  in
  case Result.map3 (,,) remoteReleases remoteReadme remoteDocs of
    Err err ->
      Problem (Problem.BadResource err)

    Ok (Just releases, Just readme, Just docs) ->
      case isModuleProblem maybeModule docs of
        Just moduleName ->
          Problem (Problem.RemovedModule user project vsn moduleName)

        Nothing ->
          Done readme docs

    Ok (_, maybeReadme, maybeDocs) ->
      Loading (Data info3) maybeReadme maybeDocs <|
        Cmd.map Load (Cmd.batch [ cmd1, cmd2, cmd3 ])


isModuleProblem : Maybe String -> List Docs.Module -> Maybe String
isModuleProblem maybeModule docsList =
  case maybeModule of
    Nothing ->
      Nothing

    Just moduleName ->
      if List.any (\docs -> moduleName == docs.name) docsList then
        Nothing

      else
        Just moduleName



-- LOAD HELPERS


type alias Step a =
  ( Info, Cmd Resource.Result, Remote a )


loadReleases : String -> String -> Info -> Step (List Release.Release)
loadReleases user project info =
  let key = toPkgKey user project in
  case Dict.get key info.releases of
    Just releases ->
      ( info, Cmd.none, releases )

    Nothing ->
      ( { info | releases = Dict.insert key loading info.releases }
      , Resource.getReleases user project
      , loading
      )


loadReadme : String -> String -> Route.Version -> Info -> Step String
loadReadme user project version info =
  let key = toVsnKey user project version in
  case Dict.get key info.readmes of
    Just readme ->
      ( info, Cmd.none, readme )

    Nothing ->
      ( { info | readmes = Dict.insert key loading info.readmes }
      , Resource.getReadme user project version
      , loading
      )


loadDocs : String -> String -> Route.Version -> Info -> Step (List Docs.Module)
loadDocs user project version info =
  let key = toVsnKey user project version in
  case Dict.get key info.docs of
    Just docs ->
      ( info, Cmd.none, docs )

    Nothing ->
      ( { info | docs = Dict.insert key loading info.docs }
      , Resource.getDocs user project version
      , loading
      )



-- KEYS


toPkgKey : String -> String -> String
toPkgKey user project =
  user ++ "/" ++ project


toVsnKey : String -> String -> Route.Version -> String
toVsnKey user project vsn =
  user ++ "/" ++ project ++ "/" ++ Route.vsnToString vsn
