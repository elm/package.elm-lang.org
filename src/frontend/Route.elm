module Route exposing
  ( Route(..)
  , Version(..)
  , fromLocation
  , toString
  )


import UrlParser exposing (Parser, (</>), map, s)
import Version



-- ROUTE


type Route
  = Home
  | User String
  | Package String String
  | Version String String Version
  | Module String String Version String (Maybe String)
  | Guidelines
  | DocsHelp


type Version
  = Latest
  | Exactly Version.Version



-- LOCATION TO ROUTE


fromLocation : Location -> Maybe Route
fromLocation location =
  case UrlParser.parsePath location of
    Just (Module user project vsn moduleName _) as result ->
      if String.isEmpty location.hash then
        result
      else
        Just (Module user project vsn moduleName (Just (String.dropLeft 1 location.hash)))

    result ->
      result


parser : Parser (Route -> a) a
parser =
  UrlParser.oneOf
    [ map Home        <| top
    , map User        <| s "packages" </> user
    , map Package     <| s "packages" </> user </> project
    , map Version     <| s "packages" </> user </> project </> version
    , map Module      <| s "packages" </> user </> project </> version </> moduleName </> map Nothing top
    , map Guidelines  <| s "help" </> s "design-guidelines"
    , map DocsHelp    <| s "help" </> s "docs"
    ]


user : Parser (String -> a) a
user =
  UrlParser.custom "USER" Ok


project : Parser (String -> a) a
project =
  UrlParser.custom "PROJECT" Ok


version : Parser (Version -> a) a
version =
  UrlParser.custom "VERSION" <| \string ->
    if string == "latest" then
      Just Latest
    else
      Maybe.map Exactly (Version.fromString string)


moduleName : Parser (String -> a) a
moduleName =
  UrlParser.custom "MODULE" (Ok << String.replace "-" ".")



-- ROUTE TO STRING


toString : Route -> String
toString route =
  case route of
    Home ->
      "/"

    User user ->
      "/packages/" ++ user ++ "/"

    Package user project ->
      "/packages/" ++ user ++ "/" ++ project ++ "/"

    Version user project vsn ->
      "/packages/" ++ user ++ "/" ++ project ++ "/" ++ vsnToString vsn ++ "/"

    Module user project vsn moduleName maybeValue ->
      let
        path =
          "/packages/" ++ user
          ++ "/" ++ project
          ++ "/" ++ vsnToString vsn
          ++ "/" ++ String.replace "." "-" moduleName
      in
        case maybeValue of
          Nothing ->
            path

          Just value ->
            path ++ "#" ++ value

    Guidelines ->
      "/help/design-guidelines"

    DocsHelp ->
      "/help/docs"


vsnToString : Version -> String
vsnToString vsn =
  case vsn of
    Latest ->
      "latest"

    Exactly version ->
      Version.toString version
