{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Task
  ( Task
  , run
  , fetchGithub
  )
  where


import Control.Monad.Except (ExceptT, runExceptT, liftIO, throwError)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

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



-- HTTP


fetchGithub :: Decode.Decoder a -> String -> Task a
fetchGithub decoder path =
  do  token <- ask
      result <- liftIO $ Http.fetchGithub token path
      case result of
        Left err ->
          throwError err

        Right bytestring ->
          case Decode.parse decoder bytestring of
            Left _ ->
              throwError $ "Bad JSON from GitHub for " ++ path

            Right value ->
              return value
