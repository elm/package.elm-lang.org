module Version exposing
  ( Version(..)
  , toString
  , fromString
  , sameMajor
  , decoder
  , encode
  )


import Json.Decode as Decode
import Json.Encode as Encode



-- VERSIONS


type Version =
  Version Int Int Int



-- STRINGS


toString : Version -> String
toString (Version major minor patch) =
  String.fromInt major
  ++ "." ++ String.fromInt minor
  ++ "." ++ String.fromInt patch


fromString : String -> Maybe Version
fromString string =
  case String.split "." string of
    [major, minor, patch] ->
      Maybe.map3 Version
        (String.toInt major)
        (String.toInt minor)
        (String.toInt patch)

    _ ->
      Nothing



-- COMPARISON


sameMajor : Version -> Version -> Bool
sameMajor (Version major1 _ _) (Version major2 _ _) =
  major1 == major2



-- JSON


decoder : Decode.Decoder Version
decoder =
  let
    toVersion string =
      case fromString string of
        Nothing ->
          Decode.fail <| "expecting a version like 2.0.1"

        Just version ->
          Decode.succeed version
  in
    Decode.andThen toVersion Decode.string


encode : Version -> Encode.Value
encode version =
  Encode.string (toString version)
