module Route exposing
  ( Route(..)
  , Version(..)
  , VersionInfo(..)
  , fromLocation
  , toString
  , vsnToString
  )


import Url.Builder as B
import Url.Parser as Url exposing (Parser, (</>), map, s, fragment)
import Version



-- ROUTE


type Route
  = Home
  | User String
  | Package String String
  | Version String String Version VersionInfo
  | Guidelines
  | DocsHelp


type Version
  = Latest
  | Exactly Version.Version


type VersionInfo
  = Readme
  | Module String (Maybe String)



-- LOCATION TO ROUTE


fromLocation : Location -> Maybe Route
fromLocation location =
  Url.parseLocation parser
    (Url.preparePath location.pathname)
    (Url.prepareQuery location.search)
    (Url.prepareFragment location.hash)


parser : Parser (Route -> a) a
parser =
  Url.oneOf
    [ map Home        <| Url.top
    , map User        <| s "packages" </> user
    , map Package     <| s "packages" </> user </> project
    , map Version     <| s "packages" </> user </> project </> version </> versionInfo
    , map Guidelines  <| s "help" </> s "design-guidelines"
    , map DocsHelp    <| s "help" </> s "docs"
    ]


user : Parser (String -> a) a
user =
  Url.custom "USER" Just


project : Parser (String -> a) a
project =
  Url.custom "PROJECT" Just


version : Parser (Version -> a) a
version =
  Url.custom "VERSION" <| \string ->
    if string == "latest" then
      Just Latest
    else
      Maybe.map Exactly (Version.fromString string)


versionInfo : Parser (VersionInfo -> a) a
versionInfo =
  Url.oneOf
    [ map Readme Url.top
    , map Module (moduleName </> Url.fragment identity)
    ]


moduleName : Parser (String -> a) a
moduleName =
  Url.custom "MODULE" (Just << String.replace "-" ".")



-- ROUTE TO STRING


toString : Route -> String
toString route =
  case route of
    Home ->
      B.absolute [] []

    User user ->
      B.absolute [ "packages", user ] []

    Package user project ->
      B.absolute [ "packages", user, project ] []

    Version user project vsn Readme ->
      B.absolute [ "packages", user, project, vsnToString vsn ] []

    Version user project vsn (Module moduleName maybeValue) ->
      B.custom B.Absolute
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


vsnToString : Version -> String
vsnToString vsn =
  case vsn of
    Latest ->
      "latest"

    Exactly version ->
      Version.toString version
