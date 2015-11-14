module Page.Context where

import Http
import Task
import Docs.Package as Docs


type alias Context =
    { user : String
    , project : String
    , version : String
    , allVersions : List String
    , moduleName : Maybe String
    }


(</>) a b =
    a ++ "/" ++ b


getDocs : Context -> Task.Task Http.Error Docs.Package
getDocs {user,project,version} =
  let
    url =
      "/packages" </> user </> project </> version </> "documentation.json"
  in
    Http.get Docs.decodePackage url
