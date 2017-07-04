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


import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans (lift, liftIO)
import System.Directory (removeDirectoryRecursive)
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
  do  result <- lift $ runExceptT transaction
      case result of
        Right a ->
          return a

        Left (Error msg) ->
          do  let version = Pkg.versionToString vsn
              let dir = "packages" </> Pkg.toFilePath pkg </> version
              let err = "Bailed out of " ++ Pkg.toString pkg ++ " " ++ version ++ " transaction."
              liftIO $ removeDirectoryRecursive dir
              liftIO $ hPutStrLn stderr err
              throwError msg



-- HTTP


fetchGithub :: Decode.Decoder a -> String -> Transaction a
fetchGithub decoder path =
  do  token <- ask
      result <- liftIO $ Http.fetchGithub token path
      case result of
        Left msg ->
          bail msg

        Right bytestring ->
          case Decode.parse decoder bytestring of
            Left _ ->
              bail ("Bad JSON from GitHub for " ++ path)

            Right value ->
              return value
