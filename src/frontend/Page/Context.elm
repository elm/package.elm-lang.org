module Page.Context where

import Http
import Task
import Docs.Package as Docs
import Utils.Path exposing ((</>))


type alias Context =
    { user : String
    , project : String
    , version : String
    , allVersions : List String
    , moduleName : Maybe String
    }


getReadme : Context -> Task.Task Http.Error String
getReadme context =
  Http.getString (pathTo context "README.md")


getDocs : Context -> Task.Task Http.Error Docs.Package
getDocs context =
  Http.get Docs.decodePackage (pathTo context "documentation.json")


pathTo : Context -> String -> String
pathTo {user,project,version} file =
  "/packages" </> user </> project </> version </> file