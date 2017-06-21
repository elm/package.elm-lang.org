{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import GHC.Conc
import Snap.Http.Server (httpServe, setPort, defaultConfig)
import System.Console.CmdArgs
import System.Directory (doesFileExist, createDirectoryIfMissing)

import qualified Artifacts
import qualified GitHub
import qualified Memory
import qualified Server




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
      token <- GitHub.init (github cargs)

      let config = setPort (port cargs) defaultConfig
      httpServe config (Server.serve token memory)



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
