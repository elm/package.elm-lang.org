{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where


import Data.Foldable (asum)
import qualified Data.Map as Map
import Data.Text (Text)
import GHC.Conc
import qualified Snap.Core as S
import qualified Snap.Http.Server as S
import Snap.Util.FileServe (serveFile, serveDirectory)
import System.Console.CmdArgs

import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg
import qualified Json.Encode as Encode

import qualified Artifacts
import qualified Http
import qualified Memory
import qualified Memory.History as History
import qualified Package.Path as Path
import qualified Package.Register as Register
import qualified Server.Router as Router
import Server.Router (Route, top, s, int, text, (</>), (==>))
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
      token <- Http.init (github cargs)

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


serve :: Http.Token -> Memory.Memory -> S.Snap ()
serve token memory =
  asum
    [
      -- NORMAL ROUTES
      Router.serve $ Router.oneOf $
        [ top ==> ServeFile.misc "Elm Packages"
        , s "packages" ==> S.redirect' "/" 301
        , s "packages" </> text </> text ==> serveProject memory
        , s "packages" </> text </> text </> s "releases.json" ==> serveReleases
        , s "packages" </> text </> text </> version </> info ==> serveVersion memory
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


version :: Route (Pkg.Version -> a) a
version =
  Router.custom Pkg.versionFromText


data Info
  = Readme
  | Module Text


info :: Route (Info -> a) a
info =
  Router.oneOf
    [ top ==> Readme
    , text ==> Module
    ]



-- NEW PACKAGES


serveNewPackages :: Memory.Memory -> Int -> S.Snap ()
serveNewPackages memory index =
  do  history <- Memory.getHistory memory
      S.writeBuilder $ Encode.encodeUgly $ Encode.list History.encodeEvent $
        History.since index history



-- PACKAGES


serveProject :: Memory.Memory -> Text -> Text -> S.Snap ()
serveProject memory author project =
  do  let name = Pkg.Name author project
      pkgs <- Memory.getPackages memory
      if Map.member name pkgs
        then ServeFile.project name
        else S.pass


serveReleases :: Text -> Text -> S.Snap ()
serveReleases author project =
  serveFile (Path.releases (Pkg.Name author project))


serveVersion :: Memory.Memory -> Text -> Text -> Pkg.Version -> Info -> S.Snap ()
serveVersion memory author project version info =
  do  let name = Pkg.Name author project
      pkgs <- Memory.getPackages memory
      case Map.lookup name pkgs of
        Nothing ->
          S.pass

        Just (Memory.Summary versions _ _) ->
          if notElem version versions
          then S.pass
          else
            case info of
              Readme ->
                ServeFile.version name version Nothing

              Module asset ->
                serveVersionHelp name version asset



serveVersionHelp :: Pkg.Name -> Pkg.Version -> Text -> S.Snap ()
serveVersionHelp name version asset =
  case asset of
    "endpoint.json" ->
      serveFile (Path.directory name version ++ "/endpoint.json")

    "elm.json" ->
      serveFile (Path.directory name version ++ "/elm.json")

    "docs.json" ->
      serveFile (Path.directory name version ++ "/docs.json")

    "README.md" ->
      serveFile (Path.directory name version ++ "/README.md")

    _ ->
      case Module.fromHyphenPath asset of
        Just moduleName ->
          ServeFile.version name version (Just moduleName)

        Nothing ->
          S.pass
