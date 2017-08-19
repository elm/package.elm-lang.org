module Route exposing
  ( Route(..)
  , Version(..)
  , VersionInfo(..)
  , fromUrl
  , toString
  , vsnToString
  )


import Browser
import Url
import Url.Parser exposing (Parser, (</>), custom, fragment, map, oneOf, parsePath, s, top)
import Version



-- ROUTE


type Route
  = Home
  | User String
  | Package String String
  | Version String String Version VersionInfo
  | Guidelines
  | DocsHelp
  | NotFound String


type Version
  = Latest
  | Exactly Version.Version


type VersionInfo
  = Readme
  | Module String (Maybe String)



-- LOCATION TO ROUTE


fromUrl : Browser.Url -> Route
fromUrl url =
  parsePath parser NotFound url


parser : Parser (Route -> a) a
parser =
  oneOf
    [ map Home       <| top
    , map User       <| s "packages" </> user
    , map Package    <| s "packages" </> user </> project
    , map Version    <| s "packages" </> user </> project </> version </> versionInfo
    , map Guidelines <| s "help" </> s "design-guidelines"
    , map DocsHelp   <| s "help" </> s "docs"
    ]


user : Parser (String -> a) a
user =
  custom "USER" Just


project : Parser (String -> a) a
project =
  custom "PROJECT" Just


version : Parser (Version -> a) a
version =
  custom "VERSION" <| \string ->
    if string == "latest" then
      Just Latest
    else
      Maybe.map Exactly (Version.fromString string)


versionInfo : Parser (VersionInfo -> a) a
versionInfo =
  oneOf
    [ map Readme top
    , map Module (moduleName </> fragment identity)
    ]


moduleName : Parser (String -> a) a
moduleName =
  custom "MODULE" (Just << String.replace "-" ".")



-- ROUTE TO STRING


toString : Route -> String
toString route =
  case route of
    Home ->
      Url.absolute [] []

    User user ->
      Url.absolute [ "packages", user ] []

    Package user project ->
      Url.absolute [ "packages", user, project ] []

    Version user project vsn Readme ->
      Url.absolute [ "packages", user, project, vsnToString vsn ] []

    Version user project vsn (Module moduleName maybeValue) ->
      Url.custom Url.Absolute
        [ "packages"
        , user
        , project
        , vsnToString vsn
        , String.replace "." "-" moduleName
        ]
        []
        maybeValue

    Guidelines ->
      "/help/design-guidelines"

    DocsHelp ->
      "/help/docs"

    NotFound unknownUrl ->
      unknownUrl


vsnToString : Version -> String
vsnToString vsn =
  case vsn of
    Latest ->
      "latest"

    Exactly version ->
      Version.toString version
