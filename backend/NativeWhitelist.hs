{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module NativeWhitelist (verify) where

import Control.Monad.Error
import Data.Aeson as Json
import qualified Data.ByteString.Lazy as LBS
import qualified System.Directory as Dir
import System.IO

import qualified Elm.Package.Name as Name


nativeWhitelist :: FilePath
nativeWhitelist =
    "native-whitelist.json"


readWhitelist :: IO [Name.Name]
readWhitelist =
  do  exists <- Dir.doesFileExist nativeWhitelist
      case exists of
        False -> return []
        True ->
            withBinaryFile nativeWhitelist ReadMode $ \handle ->
                do  json <- LBS.hGetContents handle
                    case Json.decode json of
                      Nothing -> return []
                      Just names -> return names


verify :: (MonadIO m, MonadError String m) => Name.Name -> m ()
verify name =
  do  whitelist <- liftIO readWhitelist
      case name `elem` whitelist of
        False -> throwError whitelistError
        True -> return ()


whitelistError :: String
whitelistError =
    "You are trying to publish a project that has native-modules. For now,\n\
    \    any modules that use Native code must go through a formal review process\n\
    \    to make sure the exposed API is pure and the Native code is absolutely\n\
    \    necessary. Please open an issue with the title \"Native review for ____\"\n\
    \    to begin the review process: <https://github.com/elm-lang/elm-get/issues>"
