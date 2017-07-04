{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module GetDates (check) where

import Control.Monad.Trans (liftIO)
import qualified Data.Time.ISO8601 as Time
import qualified Data.Time.Clock.POSIX as Time
import qualified System.Directory as Dir
import System.FilePath ((</>))

import qualified Crawl
import qualified Elm.Package as Pkg
import qualified Json.Decode as Decode
import qualified Json.Encode as Encode
import qualified Package.Path as Path
import qualified Package.Releases as Releases
import qualified Task



-- CHECK DATES


check :: [Crawl.Package] -> Task.Task ()
check packages =
  mapM_ checkReleaseDates packages


checkReleaseDates :: Crawl.Package -> Task.Task ()
checkReleaseDates (Crawl.Package pkg versions) =
  do  liftIO $ putStrLn $ "Checking " ++ Pkg.toString pkg
      releases <- traverse (getRelease pkg) versions
      liftIO $ Encode.write (Path.releases pkg) (Releases.encode releases)



-- GET RELEASE


getRelease :: Pkg.Name -> Pkg.Version -> Task.Task Releases.Release
getRelease pkg vsn =
  Task.attempt pkg vsn $
  do  let timeFile = "packages" </> Pkg.toFilePath pkg </> Pkg.versionToString vsn </> "time.dat"

      exists <- liftIO $ Dir.doesFileExist timeFile

      Releases.Release vsn <$>
        if exists then
          do  string <- liftIO $ readFile timeFile
              return $ fromIntegral (read string :: Integer)

        else
          do  sha <- Task.fetchGithub shaDecoder $ "/repos/" ++ Pkg.toUrl pkg ++ "/git/refs/tags/" ++ Pkg.versionToString vsn
              time <- Task.fetchGithub timeDecoder $ "/repos/" ++ Pkg.toUrl pkg ++ "/git/tags/" ++ sha
              liftIO $ writeFile timeFile (show (floor time :: Integer))
              return time


shaDecoder :: Decode.Decoder String
shaDecoder =
  Decode.at ["object","sha"] Decode.string


timeDecoder :: Decode.Decoder Time.POSIXTime
timeDecoder =
  do  date <- Decode.at ["tagger","date"] Decode.string
      case Time.parseISO8601 date of
        Nothing ->
          fail "Not a valid ISO 8601 date."

        Just utcTime ->
          return (1000 * Time.utcTimeToPOSIXSeconds utcTime)
