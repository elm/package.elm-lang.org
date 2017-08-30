module Release exposing
  ( Release
  , getLatestVersion
  , decoder
  )


import Json.Decode as D
import Time exposing (Time)
import Utils.OneOrMore exposing (OneOrMore(..))
import Version as Version



-- RELEASE


type alias Release =
  { version : Version.Version
  , time : Time
  }



-- GET LATEST VERSION


getLatestVersion : OneOrMore Release -> Version.Version
getLatestVersion (OneOrMore r rs) =
  getLatestVersionHelp rs r


getLatestVersionHelp : List Release -> Release -> Version.Version
getLatestVersionHelp releases maxRelease =
  case releases of
    [] ->
      maxRelease.version

    release :: otherReleases ->
      getLatestVersionHelp otherReleases <|
        if release.time > maxRelease.time then release else maxRelease



-- JSON


decoder : D.Decoder (OneOrMore Release)
decoder =
  D.keyValuePairs D.float
    |> D.andThen (decoderHelp [])


decoderHelp : List Release -> List (String, Float) -> D.Decoder (OneOrMore Release)
decoderHelp revReleases pairs =
  case pairs of
    [] ->
      case List.reverse revReleases of
        [] ->
          D.fail "Expecting at least one release!"

        r :: rs ->
          D.succeed (OneOrMore r rs)

    (str, time) :: otherPairs ->
      case Version.fromString str of
        Nothing ->
          D.fail <| "Field `" ++ str ++ "` must be a valid version, like 3.1.4"

        Just vsn ->
          decoderHelp (Release vsn time :: revReleases) otherPairs
