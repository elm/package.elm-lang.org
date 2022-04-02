module Session exposing
  ( Data
  , empty
  , getEntries
  , addEntries
  , getReleases
  , addReleases
  , fetchReleases
  , ResolvedDep
  , getResolvedDeps
  , addResolvedDeps
  , fetchResolvedDeps
  , getReadme
  , addReadme
  , fetchReadme
  , getDocs
  , addDocs
  , fetchDocs
  , getOutline
  , addOutline
  , fetchOutline
  )


import Dict
import Elm.Constraint as C
import Elm.Docs as Docs
import Elm.Package as Pkg
import Elm.Project as Outline
import Elm.Version as V
import Http
import Json.Decode as Decode
import Page.Search.Entry as Entry
import Release
import Task
import Url.Builder as Url
import Utils.OneOrMore exposing (OneOrMore(..))



-- SESSION DATA

{-| Dependencies with the most recent version that matches the constraints of a given package
-}
type alias ResolvedDep =
  { author : String
  , project : String
  , version : V.Version
  }

type alias Data =
  { entries : Maybe (List Entry.Entry)
  , releases : Dict.Dict String (OneOrMore Release.Release)
  , readmes : Dict.Dict String String
  , docs : Dict.Dict String (List Docs.Module)
  , outlines : Dict.Dict String Outline.PackageInfo
  , deps : Dict.Dict String (List ResolvedDep)
  }


empty : Data
empty =
  Data Nothing Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty



-- ENTRIES


getEntries : Data -> Maybe (List Entry.Entry)
getEntries data =
  data.entries


addEntries : List Entry.Entry -> Data -> Data
addEntries entries data =
  { data | entries = Just entries }



-- RELEASES


toPkgKey : String -> String -> String
toPkgKey author project =
  author ++ "/" ++ project


getReleases : Data -> String -> String -> Maybe (OneOrMore Release.Release)
getReleases data author project =
  Dict.get (toPkgKey author project) data.releases


addReleases : String -> String -> OneOrMore Release.Release -> Data -> Data
addReleases author project releases data =
  let
    newReleases =
      Dict.insert (toPkgKey author project) releases data.releases
  in
  { data | releases = newReleases }


fetchReleases : String -> String -> Http.Request (OneOrMore Release.Release)
fetchReleases author project =
  Http.get
    (Url.absolute [ "packages", author, project, "releases.json" ] [])
    Release.decoder


-- ResolvedDeps

getResolvedDeps : Data -> String -> String -> V.Version -> Maybe (List ResolvedDep)
getResolvedDeps data author project version =
  Dict.get (toVsnKey author project version) data.deps

addResolvedDeps : String -> String -> V.Version -> List ResolvedDep -> Data -> Data
addResolvedDeps author project version deps data =
  let
    newDeps =
      Dict.insert (toVsnKey author project version) deps data.deps
  in
  { data | deps = newDeps }

{-| Attempt to resolve the most recent version of each dependency matching
its associated constraints.

If a dependency couldn't be resolved (network issue or not available on
package.elm-lang.org) it is simply omitted from the list of resolved
dependencies. Therefore the list of resolved dependencies does not always
constitute an exhaustive list of the dependencies of a package.
-}
fetchResolvedDeps : Outline.PackageInfo -> Task.Task Never (List ResolvedDep)
fetchResolvedDeps pkgInfo =
  pkgInfo.deps
    |> List.map fetchResolvedDep
    |> Task.sequence
    |> Task.map (List.filterMap identity)

fetchResolvedDep : (Pkg.Name, C.Constraint) -> Task.Task Never (Maybe ResolvedDep)
fetchResolvedDep (pkg, constraint) =
  let mostRecentValidVersion author project releases =
        releases
          |> Utils.OneOrMore.toList
          |> List.filterMap (\release ->
              if C.check release.version constraint
              then Just release
              else Nothing)
          |> List.sortBy .time
          |> List.reverse
          |> List.head
          |> Maybe.map (\release ->
            { author = author
            , project = project
            , version = release.version
            })
  in
  case String.split "/" (Pkg.toString pkg) of
      [author,project] ->
          fetchReleases author project
            |> Http.toTask
            |> Task.map (mostRecentValidVersion author project)
            |> Task.onError (\_ -> Task.succeed Nothing) -- A dep couldn't be resolved at this time
      _ -> Task.succeed Nothing


-- README


toVsnKey : String -> String -> V.Version -> String
toVsnKey author project version =
  author ++ "/" ++ project ++ "@" ++ V.toString version


getReadme : Data -> String -> String -> V.Version -> Maybe String
getReadme data author project version =
  Dict.get (toVsnKey author project version) data.readmes


addReadme : String -> String -> V.Version -> String -> Data -> Data
addReadme author project version readme data =
  let
    newReadmes =
      Dict.insert (toVsnKey author project version) readme data.readmes
  in
  { data | readmes = newReadmes }


fetchReadme : String -> String -> V.Version -> Http.Request String
fetchReadme author project version =
  Http.getString <|
    Url.absolute [ "packages", author, project, V.toString version, "README.md" ] []



-- DOCS


getDocs : Data -> String -> String -> V.Version -> Maybe (List Docs.Module)
getDocs data author project version =
  Dict.get (toVsnKey author project version) data.docs


addDocs : String -> String -> V.Version -> List Docs.Module -> Data -> Data
addDocs author project version docs data =
  let
    newDocs =
      Dict.insert (toVsnKey author project version) docs data.docs
  in
  { data | docs = newDocs }


fetchDocs : String -> String -> V.Version -> Http.Request (List Docs.Module)
fetchDocs author project version =
  Http.get
    (Url.absolute [ "packages", author, project, V.toString version, "docs.json" ] [])
    (Decode.list Docs.decoder)



-- ELM.JSON


getOutline : Data -> String -> String -> V.Version -> Maybe Outline.PackageInfo
getOutline data author project version =
  Dict.get (toVsnKey author project version) data.outlines


addOutline : String -> String -> V.Version -> Outline.PackageInfo -> Data -> Data
addOutline author project version outline data =
  let
    newOutlines =
      Dict.insert (toVsnKey author project version) outline data.outlines
  in
  { data | outlines = newOutlines }


fetchOutline : String -> String -> V.Version -> Http.Request Outline.PackageInfo
fetchOutline author project version =
  Http.get
    (Url.absolute [ "packages", author, project, V.toString version, "elm.json" ] [])
    outlineDecoder


outlineDecoder : Decode.Decoder Outline.PackageInfo
outlineDecoder =
  Outline.decoder
    |> Decode.andThen getPkgOutline


getPkgOutline : Outline.Project -> Decode.Decoder Outline.PackageInfo
getPkgOutline outline =
  case outline of
    Outline.Application _ -> Decode.fail "Unexpected application"
    Outline.Package info  -> Decode.succeed info
