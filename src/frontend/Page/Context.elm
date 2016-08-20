module Page.Context exposing (..)

import Http
import Task
import Docs.Package as Package
import Docs.Description as Description
import Utils.Path exposing ((</>))


type alias OverviewContext =
  { user : String
  , project : String
  , versions : List String
  }


type alias VersionContext =
    { user : String
    , project : String
    , version : String
    , allVersions : List String
    , moduleName : Maybe String
    }


getReadme : VersionContext -> Task.Task Http.Error String
getReadme context =
  Http.getString (pathTo context "README.md")


getDocs : VersionContext -> Task.Task Http.Error Package.Package
getDocs context =
  Http.get Package.decodePackage (pathTo context "documentation.json")


getDescription : VersionContext -> Task.Task Http.Error Description.Description
getDescription {user,project,version} =
  let
    path = "/description?name=" ++ user </> project ++ "&version=" ++ version
  in
    Http.get Description.decoder path


pathTo : VersionContext -> String -> String
pathTo {user,project,version} file =
  "/packages" </> user </> project </> version </> file