module Session.Resource exposing
  ( getDocs
  , getReadme
  , getReleases
  , Result(..)
  , Error(..)
  )


import Elm.Docs as Docs
import Http
import Json.Decode as Decode
import Release
import Route
import Url.Builder as Url
import Version



-- RESULTS


type Result
  = Releases String String (Result Http.Error (List Release.Release))
  | Readme String String Route.Version (Result Http.Error String)
  | Docs String String Route.Version (Result Http.Error (List Docs.Module))


type Error
  = BadDocs String String Route.Version Http.Error
  | BadReadme String String Route.Version Http.Error
  | BadReleases String String Http.Error



-- HTTP GET


getReleases : String -> String -> Cmd Result
getReleases user project =
  Http.send (Releases user project) <|
    Http.get
      (Url.absolute [ "packages", user, project, "releases.json" ] [])
      Release.decoder


getReadme : String -> String -> Route.Version -> Cmd Result
getReadme user project vsn =
  Http.send (Readme user project vsn) <|
    Http.getString (toVsnUrl user project vsn "README.md")


getDocs : String -> String -> Route.Version -> Cmd Result
getDocs user project vsn =
  Http.send (Docs user project vsn) <|
    Http.get
      (toVsnUrl user project vsn "docs.json")
      (Decode.list Docs.decoder)


toVsnUrl : String -> String -> Route.Version -> String -> String
toVsnUrl user project vsn file =
  Url.absolute [ "packages", user, project, Route.vsnToString vsn, file ] []
