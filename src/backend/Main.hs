{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where


import qualified Data.ByteString as BS
import Data.Foldable (asum)
import qualified Data.Map as Map
import Data.Word (Word8)
import GHC.Conc
import qualified Snap.Core as S
import qualified Snap.Http.Server as S
import Snap.Util.FileServe (serveFile, serveDirectory)
import System.Console.CmdArgs

import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified Json.Encode as E
import qualified Parse.Primitives as P

import qualified Artifacts
import qualified GitHub
import qualified Legacy
import qualified Memory
import qualified Memory.History as History
import qualified Package.Path as Path
import qualified Package.Register as Register
import qualified Server.Router as Router
import Server.Router (Route, top, s, int, bytes, (</>), (==>))
import qualified ServeFile



-- FLAGS


data Flags =
  Flags
    { port :: Int
    , bootstrap :: Bool
    , github :: String
    }
    deriving (Data,Typeable,Show,Eq)


flags :: Flags
flags =
  Flags
    { port = 8000
        &= help "set the port of the server"
    , bootstrap = False
        &= help "avoid downloading packages from server that is currently down"
    , github = ""
        &= help "OAuth token for talking to GitHub"
    }



-- MAIN


main :: IO ()
main =
  do  setNumCapabilities =<< getNumProcessors
      cargs <- cmdArgs flags

      if bootstrap cargs
        then return ()
        else Artifacts.compile

      memory <- Memory.init
      token <- GitHub.init (github cargs)

      S.httpServe (config (port cargs)) (serve token memory)


config :: Int -> S.Config S.Snap a
config port =
  S.defaultConfig
    # S.setVerbose False
    # S.setPort port
    # S.setAccessLog S.ConfigNoLog
    # S.setErrorLog S.ConfigNoLog


(#) :: a -> (a -> b) -> b
(#) value func =
  func value



-- SERVE


serve :: GitHub.Token -> Memory.Memory -> S.Snap ()
serve token memory =
  asum
    [
      -- LEGACY USERS (<0.19)
      Legacy.getElmVersion >>= Legacy.serve
    ,
      -- NORMAL ROUTES
      Router.serve $ Router.oneOf $
        [ top ==> ServeFile.misc "Elm Packages"
        , s "packages" ==> S.redirect' "/" 301
        , s "packages" </> bytes </> bytes </> packageRoute ==> servePackageInfo memory
        , s "all-packages" ==> serveFile "all-packages.json"
        , s "all-packages" </> s "since" </> int ==> serveNewPackages memory
        , s "register" ==> Register.register token memory
        , s "help" </>
            Router.oneOf
              [ s "design-guidelines" ==> ServeFile.misc "Design Guidelines"
              , s "documentation-format" ==> ServeFile.misc "Documentation Format"
              ]
        ]
    ,
      -- STATIC STUFF
      S.route
        [ ("assets", serveDirectory "assets")
        , ("artifacts", serveDirectory "artifacts")
        , ("search.json", serveFile "search.json")
        , ("robots.txt", serveFile "robots.txt")
        , ("sitemap.xml", serveFile "sitemap.xml")
        ]
    ,
      -- NOT FOUND
      do  S.modifyResponse $ S.setResponseStatus 404 "Not Found"
          request <- S.getRequest
          if S.rqMethod request == S.GET
            then ServeFile.misc "Not Found"
            else S.writeBuilder "Not Found"

    ]



-- NEW PACKAGES


serveNewPackages :: Memory.Memory -> Int -> S.Snap ()
serveNewPackages memory index =
  do  history <- Memory.getHistory memory
      S.writeBuilder $ E.encodeUgly $ E.list History.encodeEvent $
        History.since index history



-- PACKAGE ROUTE


data PackageRoute
  = PkgOverview
  | Pkg__releases_json
  | PkgVersion (Maybe V.Version) VsnRoute


packageRoute :: Route (PackageRoute -> a) a
packageRoute =
  Router.oneOf
    [ top                  ==> PkgOverview
    , s "releases.json"    ==> Pkg__releases_json
    , version </> vsnRoute ==> PkgVersion
    ]


version :: Route (Maybe V.Version -> a) a
version =
  Router.oneOf
    [ s "latest" ==> Nothing
    , Router.custom toVersion ==> Just
    ]


toVersion :: BS.ByteString -> Maybe V.Version
toVersion bytes =
  case P.fromByteString V.parser (,) bytes of
    Right vsn -> Just vsn
    Left _    -> Nothing



-- VERSION ROUTE


data VsnRoute
  = VsnOverview
  | VsnModule ModuleName.Raw
  | Vsn__elm_json
  | Vsn__docs_json
  | Vsn__README_md
  | Vsn__endpoint_json


vsnRoute :: Route (VsnRoute -> a) a
vsnRoute =
  Router.oneOf
    [ top ==> VsnOverview
    , s "elm.json" ==> Vsn__elm_json
    , s "docs.json" ==> Vsn__docs_json
    , s "README.md" ==> Vsn__README_md
    , s "endpoint.json" ==> Vsn__endpoint_json
    , Router.custom toModuleName ==> VsnModule
    ]


toModuleName :: BS.ByteString -> Maybe ModuleName.Raw
toModuleName bytes =
  case P.fromByteString ModuleName.parser (,) (BS.map toDot bytes) of
    Right name -> Just name
    Left _     -> Nothing


toDot :: Word8 -> Word8
toDot word =
  if word == 0x2D {---}
  then 0x2E {-.-}
  else word



-- SERVE PACKAGE INFO


servePackageInfo :: Memory.Memory -> BS.ByteString -> BS.ByteString -> PackageRoute -> S.Snap ()
servePackageInfo memory author project pkgRoute =
  case P.fromByteString Pkg.parser (,) (BS.concat [author,"/",project]) of
    Left _ ->
      S.pass

    Right pkg ->
      case pkgRoute of
        PkgOverview ->
          do  pkgs <- Memory.getPackages memory
              if Map.member pkg pkgs
                then ServeFile.project pkg
                else S.pass

        Pkg__releases_json ->
          serveFile (Path.releases pkg)

        PkgVersion maybeVersion vsnRoute ->
          serveVersion memory pkg maybeVersion vsnRoute


serveVersion :: Memory.Memory -> Pkg.Name -> Maybe V.Version -> VsnRoute -> S.Snap ()
serveVersion memory pkg maybeVersion vsnRoute =
  do  pkgs <- Memory.getPackages memory
      case Map.lookup pkg pkgs of
        Nothing ->
          S.pass

        Just (Memory.Summary versions _ _) ->
          case verifyVersion maybeVersion versions of
            Nothing ->
              S.pass

            Just vsn ->
              case vsnRoute of
                VsnOverview        -> ServeFile.version pkg vsn Nothing
                Vsn__elm_json      -> serveFile (Path.directory pkg vsn ++ "/elm.json")
                Vsn__docs_json     -> serveFile (Path.directory pkg vsn ++ "/docs.json")
                Vsn__README_md     -> serveFile (Path.directory pkg vsn ++ "/README.md")
                Vsn__endpoint_json -> serveFile (Path.directory pkg vsn ++ "/endpoint.json")
                VsnModule name     -> ServeFile.version pkg vsn (Just name)


verifyVersion :: Maybe V.Version -> [V.Version] -> Maybe V.Version
verifyVersion maybeVersion versions =
  case versions of
    [] ->
      Nothing

    _:_ ->
      case maybeVersion of
        Nothing ->
          Just (maximum versions)

        Just version ->
          if elem version versions
          then Just version
          else Nothing
