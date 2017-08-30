module Session exposing
  ( Data
  , empty
  , Msg
  , update
  -- queries
  , Query
  , latestVersion
  , releases
  , readme
  , allDocs
  , moduleDocs
  )


import Dict
import Elm.Docs as Docs
import Http
import Json.Decode as Decode
import Page.Problem as Problem
import Release
import Route
import Session.Query as Query
import Session.Resource as Resource
import Session.Status as Status
import Utils.OneOrMore exposing (OneOrMore(..))
import Version



-- SESSION DATA


type Data = Data Info


type alias Status a =
  Status.Status Resource.Error a


type alias Info =
  { releases : Dict.Dict String (Status (OneOrMore Release.Release))
  , readmes : Dict.Dict String (Status String)
  , docs : Dict.Dict String (Status (List Docs.Module))
  }


empty : Data
empty =
  Data (Info Dict.empty Dict.empty Dict.empty)



-- KEYS


toPkgKey : String -> String -> String
toPkgKey user project =
  user ++ "/" ++ project


toVsnKey : String -> String -> Route.Version -> String
toVsnKey user project vsn =
  user ++ "/" ++ project ++ "/" ++ Route.vsnToString vsn



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


insert : Result Http.Error a -> Dict.Dict String (Status a) -> String -> (Http.Error -> Resource.Error) -> Dict.Dict String (Status a)
insert result dict key toError =
  case result of
    Err err ->
      Dict.insert key (Status.Failure (toError err) []) dict

    Ok a ->
      Dict.insert key (Status.Success a) dict



-- QUERIES


type alias Query a =
  Query.Query Data Msg Resource.Error a


latestVersion : String -> String -> Query Version.Version
latestVersion user project =
  Query.map Release.getLatestVersion (releases user project)


releases : String -> String -> Query (OneOrMore Release.Release)
releases user project =
  Query.required <|
    \(Data info as data) ->
      let key = toPkgKey user project in
      case Dict.get key info.releases of
        Just status ->
          ( data, Cmd.none, status )

        Nothing ->
          ( Data { info | releases = Dict.insert key Status.Loading info.releases }
          , Cmd.map Load (Resource.getReleases user project)
          , Status.Loading
          )


readme : String -> String -> Route.Version -> Query String
readme user project version =
  Query.required <|
    \(Data info as data) ->
      let key = toVsnKey user project version in
      case Dict.get key info.readmes of
        Just status ->
          ( data, Cmd.none, status )

        Nothing ->
          ( Data { info | readmes = Dict.insert key Status.Loading info.readmes }
          , Cmd.map Load (Resource.getReadme user project version)
          , Status.Loading
          )


allDocs : String -> String -> Route.Version -> Query (List Docs.Module)
allDocs user project version =
  Query.required <|
    \(Data info as data) ->
      let key = toVsnKey user project version in
      case Dict.get key info.docs of
        Just status ->
          ( data, Cmd.none, status )

        Nothing ->
          ( Data { info | docs = Dict.insert key Status.Loading info.docs }
          , Cmd.map Load (Resource.getDocs user project version)
          , Status.Loading
          )


moduleDocs : String -> String -> Route.Version -> String -> Query (Docs.Module, List Docs.Module)
moduleDocs user project version moduleName =
  Query.andThen
    (findModule user project version moduleName)
    (allDocs user project version)


findModule : String -> String -> Route.Version -> String -> List Docs.Module -> Query (Docs.Module, List Docs.Module)
findModule user project version name docsList =
  case findModuleHelp name docsList of
    Just docs ->
      Query.success (docs, docsList)

    Nothing ->
      Query.failure (Resource.MissingModule user project version name)


findModuleHelp : String -> List Docs.Module -> Maybe Docs.Module
findModuleHelp name docsList =
  case docsList of
    [] ->
      Nothing

    docs :: otherDocs ->
      if docs.name == name then
        Just docs
      else
        findModuleHelp name otherDocs
