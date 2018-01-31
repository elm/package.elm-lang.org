module Route exposing
  ( Route(..)
  , Version(..)
  , VersionInfo(..)
  , getHash
  , latest
  , exactly
  , fromUrl
  , toUrl
  , vsnToString
  )


import Url
import Url.Parser as Parser exposing (Parser, (</>), custom, fragment, map, oneOf, s, top)
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


latest : String -> String -> Route
latest user project =
  Version user project Latest Readme


exactly : String -> String -> Version.Version -> Route
exactly user project version =
  Version user project (Exactly version) Readme


getHash : VersionInfo -> Maybe String
getHash info =
  case info of
    Readme ->
      Nothing

    Module _ maybeName ->
      maybeName



-- LOCATION TO ROUTE


fromUrl : Parser.Url -> Route
fromUrl url =
  case Parser.parse parser url of
    Nothing ->
      NotFound (Parser.fromUrl url)

    Just route ->
      route


parser : Parser (Route -> a) a
parser =
  oneOf
    [ map Home       <| top
    , map User       <| s "packages" </> user_
    , map Package    <| s "packages" </> user_ </> project_
    , map Version    <| s "packages" </> user_ </> project_ </> version_ </> versionInfo
    , map Guidelines <| s "help" </> s "design-guidelines"
    , map DocsHelp   <| s "help" </> s "docs"
    ]


user_ : Parser (String -> a) a
user_ =
  custom "USER" Just


project_ : Parser (String -> a) a
project_ =
  custom "PROJECT" Just


version_ : Parser (Version -> a) a
version_ =
  custom "VERSION" <| \string ->
    if string == "latest" then
      Just Latest
    else
      Maybe.map Exactly (Version.fromString string)


versionInfo : Parser (VersionInfo -> a) a
versionInfo =
  oneOf
    [ map Readme top
    , map Module (moduleName_ </> fragment identity)
    ]


moduleName_ : Parser (String -> a) a
moduleName_ =
  custom "MODULE" (Just << String.replace "-" ".")



-- ROUTE TO URL


toUrl : Route -> String
toUrl route =
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
