{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Control.Monad (forM_)
import Control.Monad.Trans (liftIO)
import System.Console.CmdArgs
import qualified System.Directory as Dir

import qualified Crawl
import qualified Elm.Package as Pkg
import qualified GetDates as Dates
import qualified Task
import qualified MoveDocs as Docs
import qualified MoveElmJson as ElmJson
import qualified MoveReadme as Readme




-- FLAGS


data Flags =
  Flags
    { batch :: Int
    , github :: String
    }
    deriving (Data,Typeable,Show,Eq)


flags :: Flags
flags =
  Flags
    { batch = 10
        &= help "Maximum number of packages to process"
    , github = ""
        &= help "OAuth token for talking to GitHub"
    }



-- MAIN


main :: IO ()
main =
  do  cargs <- cmdArgs flags
      Task.run (github cargs) $
        do  liftIO $ putStrLn "---- CRAWLING DIRECTORIES ----"
            newPackages <- take (batch cargs) <$> Crawl.newPackages

            liftIO $ putStrLn "---- MOVING ASSETS ----"
            mapM_ moveAssets newPackages

            liftIO $ putStrLn "---- CHECKING RELEASE DATES ----"
            Dates.get =<< Crawl.upgradingPackages

            liftIO $ putStrLn "---- CHECKING ENDPOINTS ----"
            liftIO $ putStrLn "this is not implemented yet"


moveAssets :: Crawl.Package -> Task.Task ()
moveAssets (Crawl.Package pkg vsns) =
  forM_ vsns $ \vsn ->
    Task.attempt pkg vsn $
      do  liftIO $ putStrLn $ Pkg.toString pkg ++ " " ++ Pkg.versionToString vsn
          liftIO $ Dir.createDirectoryIfMissing True (Crawl.newDir pkg vsn)
          Readme.move pkg vsn
          ElmJson.move pkg vsn
          Docs.move pkg vsn
