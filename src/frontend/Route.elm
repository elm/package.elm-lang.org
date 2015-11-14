module Route where


type Route
    = Home
    | Help
    | Packages (Maybe UserRoute)


type UserRoute =
    User String (Maybe PackageRoute)


type PackageRoute =
    Package String (Maybe String)


dummy =
  Packages <| Just <|
    User "elm-lang" <| Just <|
      Package "core" <| Just <|
        "3.0.0"