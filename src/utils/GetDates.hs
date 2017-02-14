{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Control.Exception as E
import Control.Monad
import Control.Monad.Except (ExceptT, runExceptT, liftIO, throwError)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Data.Aeson ((.=), (.:))
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy as BS
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Time.ISO8601 as Time
import qualified Data.Time.Clock.POSIX as Time
import qualified System.Directory as Dir
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

import Network (withSocketsDo)
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http



-- MAIN


main :: IO ()
main =
  run checkPackages


checkPackages :: Task ()
checkPackages =
  do  users <- getSubDirs "packages"
      forM_ users $ \user ->
        do  projects <- getSubDirs ("packages" </> user)
            mapM (checkPackage user) projects


checkPackage :: String -> String -> Task ()
checkPackage user project =
  do  liftIO $ putStrLn $ "Checking " ++ user ++ "/" ++ project
      let dir = "packages" </> user </> project
      versions <- getSubDirs dir

      let noKnownTime vsn =
            not <$> Dir.doesFileExist (dir </> vsn </> "time.dat")

      timeless <- filterM (liftIO . noKnownTime) versions

      when (not (null timeless)) $
        do  tags <- fetch $ "https://api.github.com/repos/" ++ user ++ "/" ++ project ++ "/tags?per_page=100"

            forM_ (tags :: [Tag]) $ \(Tag name url) ->
              when (elem name timeless) $
                do  liftIO $ putStrLn $ "  - " ++ name
                    (Date time) <- fetch url
                    let timeFile = dir </> name </> "time.dat"
                    liftIO $ BS.writeFile timeFile (Json.encode time)

            pairs <-
              forM versions $ \vsn ->
                do  let timeFile = dir </> vsn </> "time.dat"
                    bytes <- liftIO $ BS.readFile timeFile
                    case Json.decode bytes of
                      Nothing ->
                        throwError $ "Bad time in " ++ timeFile

                      Just time ->
                        return (Text.pack vsn .= (time :: Time.POSIXTime))

            liftIO $ BS.writeFile (dir </> "overview.json") $
              Json.encode (Json.object pairs)


data Tag = Tag { _name :: String, _url :: String }


instance Json.FromJSON Tag where
  parseJSON =
    Json.withObject "Tag" $ \tag ->
      do  name <- tag .: "name"
          commit <- tag .: "commit"
          url <- commit .: "url"
          return (Tag name url)


newtype Date = Date Time.POSIXTime


instance Json.FromJSON Date where
  parseJSON =
    Json.withObject "Date" $ \object ->
      do  commit <- object .: "commit"
          author <- commit .: "author"
          date <- author .: "date"
          case Time.parseISO8601 date of
            Nothing ->
              fail "Not a valid ISO 8601 date."

            Just utcTime ->
              return (Date (1000 * Time.utcTimeToPOSIXSeconds utcTime))



-- TASKS


type Task =
  ExceptT String (ReaderT Http.Manager IO)


run :: Task a -> IO ()
run task =
  Network.withSocketsDo $
    do  httpManager <- Http.newManager Http.tlsManagerSettings
        result <- runReaderT (runExceptT task) httpManager
        case result of
          Right _ ->
            putStrLn "Success!"

          Left msg ->
            do  hPutStrLn stderr msg
                exitFailure



-- DIRECTORIES


getSubDirs :: FilePath -> Task [FilePath]
getSubDirs dir =
  liftIO $
    do  contents <- Dir.getDirectoryContents dir
        reverse <$> foldM (addSubDir dir) [] contents


addSubDir :: FilePath -> [FilePath] -> FilePath -> IO [FilePath]
addSubDir dir subs subDir =
  do  let path = dir </> subDir
      exists <- Dir.doesDirectoryExist path
      if exists && not (List.isPrefixOf "." subDir)
        then return (subDir : subs)
        else return subs



-- HTTP


fetch :: (Json.FromJSON a) => String -> Task a
fetch url =
  do  manager <- ask
      result <- liftIO (fetchHelp url manager `E.catch` badRequest url)
      either throwError return result


fetchHelp :: (Json.FromJSON a) => String -> Http.Manager -> IO (Either String a)
fetchHelp url manager =
  do  request <- Http.parseUrlThrow url
      response <- Http.httpLbs (addHeaders request) manager
      return $ Json.eitherDecode $ Http.responseBody response


addHeaders :: Http.Request -> Http.Request
addHeaders request =
  let
    auth =
      ( "Authorization", "token 0000000000000000000000000000000000000000" )

    agent =
      ( "user-agent", "elm-package-release-date-finder" )
  in
    request
      { Http.requestHeaders =
          auth : agent : Http.requestHeaders request
      }


badRequest :: String -> E.SomeException -> IO (Either String a)
badRequest url _ =
  return $ Left $ "Failure: <" ++ url ++ ">"

