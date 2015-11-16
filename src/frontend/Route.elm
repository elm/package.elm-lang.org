module Route where

import Page.Context as Ctx


type Route
    = Help
    | Packages (Maybe UserRoute)


type UserRoute =
    User String (Maybe PackageRoute)


type PackageRoute =
    Package String (Maybe VersionRoute)


type VersionRoute =
    Version String (List String) (Maybe String)


fromContext : Ctx.Context -> Route
fromContext ctx =
  Packages => User ctx.user => Package ctx.project => Version ctx.version ctx.allVersions ctx.moduleName


(=>) f a =
  f (Just a)


infixr 0 =>