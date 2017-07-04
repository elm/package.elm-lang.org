{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Control.Monad.Trans (liftIO)
import System.Console.CmdArgs

import qualified Crawl
import qualified GetDates
import qualified Task
import qualified UpdateJson




-- FLAGS


data Flags =
  Flags
    { github :: String
    }
    deriving (Data,Typeable,Show,Eq)


flags :: Flags
flags =
  Flags
    { github = ""
        &= help "OAuth token for talking to GitHub"
    }



-- MAIN


main :: IO ()
main =
  do  cargs <- cmdArgs flags
      Task.run (github cargs) $
        do  newPackages <- Crawl.newPackages

            UpdateJson.update packages

            GetDates.check packages
