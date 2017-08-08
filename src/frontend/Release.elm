module Release exposing
  ( Release
  , getLatestVersion
  , decoder
  )


import Json.Decode as D
import Time exposing (Time)
import Version as Version



-- RELEASE


type alias Release =
  { version : Version.Version
  , time : Time
  }



-- GET LATEST VERSION


getLatestVersion : List Release -> Maybe Version.Version
getLatestVersion allReleases =
  case allReleases of
    [] ->
      Nothing

    r :: rs ->
      Just (getLatestVersionHelp rs r)


getLatestVersionHelp : List Release -> Release -> Version.Version
getLatestVersionHelp releases maxRelease =
  case releases of
    [] ->
      maxRelease.version

    release :: otherReleases ->
      getLatestVersionHelp otherReleases <|
        if release.time > maxRelease.time then release else maxRelease



-- JSON


decoder : D.Decoder (List Release)
decoder =
  D.keyValuePairs D.float
    |> D.andThen (decoderHelp [])


decoderHelp : List Release -> List (String, Float) -> D.Decoder (List Release)
decoderHelp revReleases pairs =
  case pairs of
    [] ->
      D.succeed (List.reverse revReleases)

    (str, time) :: otherPairs ->
      case Version.fromString str of
        Nothing ->
          D.fail <| "Field `" ++ str ++ "` must be a valid version, like 3.1.4"

        Just vsn ->
          decoderHelp (Release vsn time :: revReleases) otherPairs
