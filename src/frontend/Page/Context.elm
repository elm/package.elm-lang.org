module Page.Context exposing (..)

import Http
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


getReadme : VersionContext -> Http.Request String
getReadme context =
  Http.getString (pathTo context "README.md")


getDocs : VersionContext -> Http.Request Docs.Package
getDocs context =
  Http.get (pathTo context "documentation.json") Docs.decodePackage


pathTo : VersionContext -> String -> String
pathTo {user,project,version} file =
  "/packages" </> user </> project </> version </> file