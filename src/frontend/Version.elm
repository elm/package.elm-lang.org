module Version exposing
  ( Version
  , toString
  , fromString
  , decoder
  , encode
  )



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
        (String.fromInt major)
        (String.fromInt minor)
        (String.fromInt patch)

    _ ->
      Nothing



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
