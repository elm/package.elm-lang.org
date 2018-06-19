module Release exposing
  ( Release
  , getLatest
  , decoder
  )


import Elm.Version as V
import Json.Decode as D
import Utils.OneOrMore exposing (OneOrMore(..))



-- RELEASE


type alias Release =
  { version : V.Version
  , time : Int
  }



-- GET LATEST VERSION


getLatest : OneOrMore Release -> V.Version
getLatest (OneOrMore r rs) =
  getLatestVersionHelp rs r


getLatestVersionHelp : List Release -> Release -> V.Version
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
  D.keyValuePairs D.int
    |> D.andThen (decoderHelp [])


decoderHelp : List Release -> List (String, Int) -> D.Decoder (OneOrMore Release)
decoderHelp revReleases pairs =
  case pairs of
    [] ->
      case List.reverse revReleases of
        [] ->
          D.fail "Expecting at least one release!"

        r :: rs ->
          D.succeed (OneOrMore r rs)

    (str, time) :: otherPairs ->
      case V.fromString str of
        Nothing ->
          D.fail <| "Field `" ++ str ++ "` must be a valid version, like 3.1.4"

        Just vsn ->
          decoderHelp (Release vsn time :: revReleases) otherPairs
