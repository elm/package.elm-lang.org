{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Foldable (asum)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Text (Text)
import GHC.Conc
import qualified Snap.Core as S
import Snap.Http.Server (httpServe, setPort, defaultConfig)
import Snap.Util.FileServe (serveFile, serveDirectory)
import System.Console.CmdArgs
import System.Directory (doesFileExist, createDirectoryIfMissing)

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
      setupLogging
      cargs <- cmdArgs flags

      if bootstrap cargs
        then return ()
        else Artifacts.compile

      memory <- Memory.init
      token <- Http.init (github cargs)

      let config = setPort (port cargs) defaultConfig
      httpServe config (serve token memory)



-- LOGGING


setupLogging :: IO ()
setupLogging =
  do  createDirectoryIfMissing True "log"
      createIfMissing "log/access.log"
      createIfMissing "log/error.log"


createIfMissing :: FilePath -> IO ()
createIfMissing path =
  do  exists <- doesFileExist path
      if exists
        then return ()
        else writeFile path ""



-- SERVE


serve :: Http.Token -> Memory.Memory -> S.Snap ()
serve token memory =
  asum
    [
      -- NORMAL ROUTES
      Router.serve $ Router.oneOf $
        [ top ==> ServeFile.elm "Elm Packages"
        , s "packages" ==> S.redirect' "/" 301
        , s "packages" </> text </> text </> s "releases.json" ==> serveReleases
        , s "packages" </> text </> text </> versionStuff ==> serveVersion memory
        , s "all-packages" ==> serveFile "all-packages.json"
        , s "all-packages" </> s "since" </> int ==> serveNewPackages memory
        , s "register" ==> Register.register token memory
        , s "help" </>
            Router.oneOf
              [ s "design-guidelines" ==> ServeFile.elm "Design Guidelines"
              , s "documentation-format" ==> ServeFile.elm "Documentation Format"
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
          ServeFile.elm "Not Found"
    ]



-- NEW PACKAGES


serveNewPackages :: Memory.Memory -> Int -> S.Snap ()
serveNewPackages memory index =
  do  history <- Memory.getHistory memory
      S.writeBuilder $ Encode.encodeUgly $ Encode.list History.encodeEvent $
        History.since index history



-- PACKAGES


data PkgInfo
  = Readme
  | Module Vsn (Maybe Text)


data Vsn = Latest | Exactly Pkg.Version


versionRoute :: Route (Pkg.Version -> a) a
versionRoute =
  Router.custom Pkg.versionFromText


versionStuff :: Route (PkgInfo -> a) a
versionStuff =
  let
    latest = s "latest" ==> Latest
    exactly = versionRoute ==> Exactly
    asset = Router.oneOf [ top ==> Nothing, text ==> Just ]
  in
    Router.oneOf
      [ top ==> Readme
      , latest </> asset ==> Module
      , exactly </> asset ==> Module
      ]


serveReleases :: Text -> Text -> S.Snap ()
serveReleases user project =
  serveFile (Path.releases (Pkg.Name user project))


serveVersion :: Memory.Memory -> Text -> Text -> PkgInfo -> S.Snap ()
serveVersion memory user project info =
  do  let name = Pkg.Name user project

      pkgs <- Memory.getPackages memory

      case Map.lookup name pkgs of
        Nothing ->
          S.pass

        Just (Memory.Summary versions _ _) ->
          case info of
            Readme ->
              ServeFile.elm (Pkg.toString name)

            Module (Exactly version) asset ->
              if notElem version versions then
                S.pass
              else
                serveVersionHelp name version asset

            Module Latest asset ->
              serveVersionHelp name (last (List.sort versions)) asset


serveVersionHelp :: Pkg.Name -> Pkg.Version -> Maybe Text -> S.Snap ()
serveVersionHelp name version maybeAsset =
  case maybeAsset of
    Nothing ->
      ServeFile.docsHtml name version Nothing

    Just "endpoint.json" ->
      serveFile (Path.directory name version ++ "/endpoint.json")

    Just "elm.json" ->
      serveFile (Path.directory name version ++ "/elm.json")

    Just "docs.json" ->
      serveFile (Path.directory name version ++ "/docs.json")

    Just "README.md" ->
      serveFile (Path.directory name version ++ "/README.md")

    Just asset ->
      case Module.fromHyphenPath asset of
        Nothing ->
          S.pass

        Just moduleName ->
          ServeFile.docsHtml name version (Just moduleName)
