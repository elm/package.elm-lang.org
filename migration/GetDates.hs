{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module GetDates (get) where

import Control.Monad.Except (catchError, throwError)
import Control.Monad.Trans (liftIO)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Time.Clock.POSIX as Time
import qualified Data.Time.ISO8601 as Time
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


get :: [Crawl.Package] -> Task.Task ()
get packages =
  mapM_ getReleaseDates packages


getReleaseDates :: Crawl.Package -> Task.Task ()
getReleaseDates (Crawl.Package pkg versions) =
  do  maybeReleases <- traverse (getRelease pkg) versions
      case sequence maybeReleases of
        Nothing ->
          liftIO $ putStrLn $ "No releases.json generated for " ++ Pkg.toString pkg

        Just releases ->
          liftIO $ Encode.write (Path.releases pkg) (Releases.encode releases)



-- GET RELEASE


getRelease :: Pkg.Name -> Pkg.Version -> Task.Task (Maybe Releases.Release)
getRelease pkg vsn =
  do  let timeFile = "packages" </> Pkg.toFilePath pkg </> Pkg.versionToString vsn </> "time.dat"

      exists <- liftIO $ Dir.doesFileExist timeFile

      maybeTime <-
        if exists then
          do  string <- liftIO $ readFile timeFile
              return (Just (fromIntegral (read string :: Integer)))
        else
          maybe (getReleaseHelp pkg vsn timeFile) (return . Just) (checkHandCollected pkg vsn)

      return (Releases.Release vsn <$> maybeTime)


getReleaseHelp :: Pkg.Name -> Pkg.Version -> FilePath -> Task.Task (Maybe Time.POSIXTime)
getReleaseHelp pkg vsn timeFile =
  do  endpoint <- getEndpoint pkg vsn
      time <- getTime pkg endpoint
      liftIO $ putStrLn $ Pkg.toString pkg ++ " " ++ Pkg.versionToString vsn
      liftIO $ writeFile timeFile (show (floor time :: Integer))
      return (Just time)
  `catchError` \err ->
    if List.isInfixOf "statusCode = 403" err then
      throwError $ "probably ran out of requests...\n\n" ++ err
    else
      do  liftIO $ putStrLn $ Pkg.toString pkg ++ " " ++ Pkg.versionToString vsn ++ " download failed"
          return Nothing



-- GET ENDPOINT


getEndpoint :: Pkg.Name -> Pkg.Version -> Task.Task Endpoint
getEndpoint pkg vsn =
  Task.fetchGithub endpointDecoder $
    "/repos/" ++ Pkg.toUrl pkg ++ "/git/refs/tags/" ++ Pkg.versionToString vsn


data Endpoint
  = Tag String
  | Commit String


endpointDecoder :: Decode.Decoder Endpoint
endpointDecoder =
  let
    toUrl tipe =
      case tipe of
        "commit" ->
          Decode.map Commit $ Decode.at ["object","sha"] Decode.string

        "tag" ->
          Decode.map Tag $ Decode.at ["object","sha"] Decode.string

        _ ->
          Decode.fail $ "unknown type: " ++ tipe
  in
    Decode.andThen toUrl $
      Decode.at ["object","type"] Decode.string



-- GET TIME


getTime :: Pkg.Name -> Endpoint -> Task.Task Time.POSIXTime
getTime pkg endpoint =
  case endpoint of
    Tag sha ->
      Task.fetchGithub (timeDecoder "tagger") $
        "/repos/" ++ Pkg.toUrl pkg ++ "/git/tags/" ++ sha

    Commit sha ->
      Task.fetchGithub (timeDecoder "committer") $
        "/repos/" ++ Pkg.toUrl pkg ++ "/git/commits/" ++ sha


timeDecoder :: Text.Text -> Decode.Decoder Time.POSIXTime
timeDecoder person =
  do  date <- Decode.at [person,"date"] Decode.string
      case Time.parseISO8601 date of
        Nothing ->
          fail "Not a valid ISO 8601 date."

        Just utcTime ->
          return (Time.utcTimeToPOSIXSeconds utcTime)



-- HAND-COLLECTED TIMES


checkHandCollected :: Pkg.Name -> Pkg.Version -> Maybe Time.POSIXTime
checkHandCollected pkg vsn =
  let name = Pkg.toString pkg ++ "@" ++ Pkg.versionToString vsn
  in
    Time.utcTimeToPOSIXSeconds <$> (Time.parseISO8601 =<< Map.lookup name handCollectedTimes)


handCollectedTimes :: Map.Map String String
handCollectedTimes =
  Map.fromList
    [ "amilner42/keyboard-extra@1.0.0" ==> "2017-02-18T03:13:59Z"
    , "elm-community/typed-svg@1.0.0" ==> "2017-04-28T09:33:37Z"
    , "Janiczek/elm-markov@2.0.1" ==> "2016-05-19T19:15:09Z"
    , "krisajenkins/elm-exts@3.2.0" ==> "2015-03-24T23:18:13Z"
    , "krisajenkins/elm-exts@4.0.0" ==> "2015-04-25T21:29:31Z"
    , "lynn/elm-arithmetic@2.0.1" ==> "2016-07-13T19:52:52Z"
    , "patrickjtoy/elm-table@2.0.1" ==> "2017-05-19T15:55:43Z"
    , "tilmans/elm-style-elements-drag-drop@1.0.1" ==> "2017-07-02T16:35:24Z"
    , "tilmans/elm-style-elements-drag-drop@1.0.2" ==> "2017-07-02T16:35:24Z"
    , "w0rm/elm-slice-show@3.0.1" ==> "2016-06-04T15:13:24Z"
    , "w0rm/elm-slice-show@3.0.0" ==> "2016-06-04T14:53:36Z"
    ]


(==>) :: a -> b -> ( a, b )
(==>) = (,)