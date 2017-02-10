{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Control.Monad (when)
import GHC.Conc
import Snap.Http.Server (httpServe, setPort, defaultConfig)
import System.Console.CmdArgs
import System.Directory (doesFileExist, createDirectoryIfMissing)

import qualified Artifacts
import qualified Memory
import qualified Server




-- FLAGS


data Flags =
  Flags
    { port :: Int
    , bootstrap :: Bool
    }
    deriving (Data,Typeable,Show,Eq)


flags :: Flags
flags =
  Flags
    { port = 8000
        &= help "set the port of the server"
    , bootstrap = False
        &= help "do not build the UI, because that needs to pull dependencies from the server we are trying to start"
    }



-- MAIN


main :: IO ()
main =
  do  setNumCapabilities =<< getNumProcessors
      setupLogging
      cargs <- cmdArgs flags

      when (not (bootstrap cargs))
        Artifacts.compile

      memory <- Memory.init

      httpServe
        (setPort (port cargs) defaultConfig)
        (Server.serve memory)



-- LOGGING


setupLogging :: IO ()
setupLogging =
  do  createDirectoryIfMissing True "log"
      createIfMissing "log/access.log"
      createIfMissing "log/error.log"


createIfMissing :: FilePath -> IO ()
createIfMissing path =
  do  exists <- doesFileExist path
      when (not exists) $ writeFile path ""
