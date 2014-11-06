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


data Flags = Flags
    { port :: Int
    }
    deriving (Data,Typeable,Show,Eq)


flags :: Flags
flags = Flags
    { port = 8000 &= help "set the port of the server"
    }


-- | Set up the server.
main :: IO ()
main =
  do  setNumCapabilities =<< getNumProcessors
      setupLogging
      compileElmFiles
      cargs <- cmdArgs flags
      httpServe (setPort (port cargs) defaultConfig) $
          route
            [ ("", pass)
            , ("packages", Route.packages)
            , ("versions", Route.versions)
            , ("register", Route.register)
            , ("description", Route.description)
            , ("documentation", Route.documentation)
            , ("all-packages", Route.allPackages)
            , ("assets", serveDirectoryWith directoryConfig "assets")
            , ( BS.pack Path.artifactDirectory
              , serveDirectoryWith directoryConfig Path.artifactDirectory
              )
            ]
          <|> do modifyResponse $ setResponseStatus 404 "Not found"
                 serveFile "public/Error404.html"


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
    [ ["Test"]
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
