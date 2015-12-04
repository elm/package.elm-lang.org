module Page.Context where

import Http
import Task
import Docs.Package as Docs
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


type alias SpecificVersion r =
  { r | user : String, project : String, version : String }


getReadme : SpecificVersion a -> Task.Task Http.Error String
getReadme context =
  Http.getString (pathTo context "README.md")


getDocs : SpecificVersion a -> Task.Task Http.Error (Docs.Package String)
getDocs context =
  Http.get Docs.decodePackage (pathTo context "documentation.json")


pathTo : SpecificVersion a -> String -> String
pathTo {user,project,version} file =
  "/packages" </> user </> project </> version </> file