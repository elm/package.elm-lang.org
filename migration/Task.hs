{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Task
  ( Task
  , run
  , Transaction
  , bail
  , attempt
  , fetchGithub
  )
  where


import Control.Exception (SomeException, catch, try)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified System.Directory as Dir
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

import qualified Elm.Package as Pkg
import qualified Http
import qualified Json.Decode as Decode



-- TASKS


type Task =
  ExceptT String (ReaderT Http.Token IO)


run :: String -> Task a -> IO ()
run githubToken task =
  do  token <- Http.init githubToken
      result <- runReaderT (runExceptT task) token
      case result of
        Right _ ->
          putStrLn "Success!"

        Left msg ->
          do  hPutStrLn stderr msg
              exitFailure



-- ERROR


type Transaction =
  ExceptT Error (ReaderT Http.Token IO)


newtype Error = Error String


bail :: String -> Transaction a
bail msg =
  throwError (Error msg)


attempt :: Pkg.Name -> Pkg.Version -> Transaction a -> Task a
attempt pkg vsn transaction =
  do  token <- ask

      result <- liftIO $ runReaderT (runExceptT transaction) token
        `catch` \e -> return (Left (Error (show (e :: SomeException))))

      case result of
        Right a ->
          return a

        Left (Error msg) ->
          do  liftIO $ hPutStrLn stderr $
                "Bailed out of " ++ Pkg.toString pkg ++ " " ++ Pkg.versionToString vsn ++ " transaction."
              liftIO $ removeDirectory pkg vsn
              throwError msg


removeDirectory :: Pkg.Name -> Pkg.Version -> IO ()
removeDirectory (Pkg.Name user project) version =
  do  let usr = Text.unpack user
      let prj = Text.unpack project
      let vsn = Pkg.versionToString version

      Dir.removeDirectoryRecursive ("packages" </> usr </> prj </> vsn)
      _ <- try $ Dir.removeDirectory ("packages" </> usr </> prj) :: IO (Either SomeException ())
      _ <- try $ Dir.removeDirectory ("packages" </> usr) :: IO (Either SomeException ())

      return ()



-- HTTP


fetchGithub :: Decode.Decoder e a -> String -> Task a
fetchGithub decoder path =
  do  token <- ask
      result <- liftIO $ Http.fetchGithub token path
      case result of
        Left msg ->
          throwError msg

        Right bytestring ->
          case Decode.parse "github" (\_ -> []) decoder (LBS.toStrict bytestring) of
            Left _ ->
              throwError ("Bad JSON from GitHub for " ++ path)

            Right value ->
              return value
