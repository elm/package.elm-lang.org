module Session exposing
  ( Data
  , empty
  , getDocs
  , getReadme
  , getLatestVersion
  , Msg
  , update
  , loadDocs
  , loadReadme
  , loadReleases
  )


import Elm.Docs as Docs
import Json.Decode as Decode
import Release
import Version



-- SESSION DATA


type Data = Data Info


type alias Info =
  { releases : Dict.Dict String (Remote (List Release.Release))
  , readmes : Dict.Dict String (Remote String)
  , docs : Dict.Dict String (Remote (List Docs.Module))
  }


type Remote a
  = Loading
  | Loaded a
  | Failed



-- HELPERS


empty : Data
empty =
  Data (Info Dict.empty Dict.empty Dict.empty)


toPkgKey : String -> String -> String
toPkgKey user project =
  user ++ "/" ++ project


toVsnKey : String -> String -> Vsn.Version -> String
toVsnKey user project version =
  user ++ "/" ++ project ++ "/" ++ Vsn.toString version


getDocs : Data -> String -> String -> Vsn.Version -> Remote (List Docs.Module)
getDocs (Data data) user project version =
  Maybe.withDefault Loading <|
    Dict.get (toVsnKey user project version) data.docs


getReadme : Data -> String -> String -> Vsn.Version -> Remote String
getReadme (Data data) user project version =
  Maybe.withDefault Loading <|
    Dict.get (toVsnKey user project version) data.readmes


getLatestVersion : Data -> String -> String -> Maybe Vsn.Version
getLatestVersion (Data data) user project =
  case Dict.get (toPkgKey user project) data.releases of
    Just (Loaded releases) ->
      Release.getLatestVersion releases

    _ ->
      Nothing



-- UPDATE


type Msg
  = LoadReleases String (Result Http.Error (List Release.Release))
  | LoadReadme String (Result Http.Error String)
  | LoadDocs String (Result Http.Error (List Docs.Module))


update : Msg -> Data -> Data
update msg (Data info) =
  case msg of
    LoadReleases key result ->
      Data { info | releases = Dict.insert key (toRemote result) info.releases }

    LoadReadme key result ->
      Data { info | readmes = Dict.insert key (toRemote result) info.readmes }

    LoadDocs key result ->
      Data { info | docs = Dict.insert key (toRemote result) info.docs }


toRemote : Result e a -> Remote a
toRemote result =
  case result of
    Err _ ->
      Failed

    Ok value ->
      Loaded value



-- HTTP


loadDocs : Data -> String -> String -> Vsn.Version -> ( Data, Cmd Msg )
loadDocs (Data info as data) user project version =
  let key = toVsnKey user project version in
  case Dict.get key info.docs of
    Just _ ->
      ( data, Cmd.none )

    Nothing ->
      let
        docsUrl =
          "/packages/" ++ user ++ "/" ++ project ++ "/" ++ Vsn.toString version ++ "/docs.json"
      in
        ( Data { info | docs = Dict.insert key Loading info.docs }
        , Http.send (LoadDocs key) <| Http.get docsUrl (Decode.list Docs.decoder)
        )


loadReadme : Data -> String -> String -> Vsn.Version -> ( Data, Cmd Msg )
loadReadme (Data info as data) user project version =
  let key = toVsnKey user project version in
  case Dict.get key info.readmes of
    Just _ ->
      ( data, Cmd.none )

    Nothing ->
      let
        readmeUrl =
          "/packages/" ++ user ++ "/" ++ project ++ "/" ++ Vsn.toString version ++ "/README.md"
      in
        ( Data { info | readmes = Dict.insert key Loading info.readmes }
        , Http.send (LoadReadme key) <| Http.getString readmeUrl
        )


loadReleases : Data -> String -> String -> ( Data, Cmd Msg )
loadReleases (Data info as data) user project =
  let key = toPkgKey user project in
  case Dict.get key info.releases of
    Just _ ->
      ( data, Cmd.none )

    Nothing ->
      let
        releasesUrl =
          "/packages/" ++ user ++ "/" ++ project ++ "/releases.json"
      in
        ( Data { info | releases = Dict.insert key Loading info.releases }
        , Http.send (LoadReleases key) <| Http.get releasesUrl Release.decoder
        )
