{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module Main where

import Control.Applicative
import Control.Monad.Error
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as Map
import GHC.Conc
import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe
import System.Console.CmdArgs
import System.Directory
import System.Exit (exitFailure)
import System.FilePath ((</>), (<.>))
import System.IO (hPutStrLn, stderr)

import Elm.Utils ((|>))
import qualified Elm.Compiler.Module as Module
import qualified Elm.Utils as Utils
import qualified Path
import qualified Routes as Route
import qualified ServeFile


data Flags = Flags
    { port :: Int
    , bootstrap :: Bool
    }
    deriving (Data,Typeable,Show,Eq)


flags :: Flags
flags = Flags
    { port = 8000 &= help "set the port of the server"
    , bootstrap = False &= help "do not build the UI, because that needs to pull dependencies from the server we are trying to start"
    }


-- | Set up the server.
main :: IO ()
main =
  do  setNumCapabilities =<< getNumProcessors
      setupLogging
      cargs <- cmdArgs flags

      when (not (bootstrap cargs))
          compileElmFiles

      httpServe (setPort (port cargs) defaultConfig) $
          ifTop (ServeFile.filler (Module.Name ["Page","Home"]))
          <|>
            route
            [ ("packages", Route.packages)
            , ("versions", Route.versions)
            , ("register", Route.register)
            , ("description", Route.description)
            , ("documentation", Route.documentation)
            , ("all-packages", Route.allPackages)
            , ("assets", serveDirectoryWith directoryConfig "assets")
            , ("help/design-guidelines", ifTop $ ServeFile.filler (Module.Name ["Page","DesignGuidelines"]))
            , ("help/documentation-format", ifTop $ ServeFile.filler (Module.Name ["Page","DocumentationFormat"]))
            , ( BS.pack Path.artifactDirectory
              , serveDirectoryWith directoryConfig Path.artifactDirectory
              )
            ]
          <|>
            do  modifyResponse $ setResponseStatus 404 "Not found"
                (ServeFile.filler (Module.Name ["Page","Error"]))


setupLogging :: IO ()
setupLogging =
  do  createDirectoryIfMissing True "log"
      createIfMissing "log/access.log"
      createIfMissing "log/error.log"
  where
    createIfMissing path =
      do  exists <- doesFileExist path
          when (not exists) $ writeFile path ""


compileElmFiles :: IO ()
compileElmFiles =
  do  createDirectoryIfMissing True Path.artifactDirectory
      result <-
        runErrorT $
            forM publicModules $ \name ->
                Utils.run "elm-make"
                    [ "frontend" </> Module.nameToPath name <.> "elm"
                    , "--yes"
                    , "--output=" ++ Path.artifact name
                    ]
      case result of
        Right _ -> return ()
        Left msg ->
          do  hPutStrLn stderr msg
              exitFailure


publicModules :: [Module.Name]
publicModules =
    map Module.Name
    [ ["Page","DesignGuidelines"]
    , ["Page","DocumentationFormat"]
    , ["Page","Error"]
    , ["Page","Home"]
    , ["Page","ModuleDocs"]
    , ["Page","PackageDocs"]
    , ["Page","Packages"]
    ]


directoryConfig :: MonadSnap m => DirectoryConfig m
directoryConfig =
    fancyDirectoryConfig
    { indexGenerator = defaultIndexGenerator defaultMimeTypes indexStyle
    , mimeTypes =
        defaultMimeTypes
          |> Map.insert ".elm" "text/plain"
          |> Map.insert ".ico" "image/x-icon"
    }


indexStyle :: BS.ByteString
indexStyle =
    "body { margin:0; font-family:sans-serif; background:rgb(245,245,245);\
    \       font-family: calibri, verdana, helvetica, arial; }\
    \div.header { padding: 40px 50px; font-size: 24px; }\
    \div.content { padding: 0 40px }\
    \div.footer { display:none; }\
    \table { width:100%; border-collapse:collapse; }\
    \td { padding: 6px 10px; }\
    \tr:nth-child(odd) { background:rgb(216,221,225); }\
    \td { font-family:monospace }\
    \th { background:rgb(90,99,120); color:white; text-align:left;\
    \     padding:10px; font-weight:normal; }"
