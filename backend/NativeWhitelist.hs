{-# LANGUAGE OverloadedStrings #-}
module NativeWhitelist (read) where

import Prelude hiding (read)
import Data.Aeson as Json
import qualified Data.ByteString.Lazy as LBS
import qualified System.Directory as Dir
import System.IO

import qualified Elm.Package.Name as Name


nativeWhitelist :: FilePath
nativeWhitelist =
    "native-whitelist.json"


read :: IO [Name.Name]
read =
  do  exists <- Dir.doesFileExist nativeWhitelist
      case exists of
        False -> return []
        True ->
            withBinaryFile nativeWhitelist ReadMode $ \handle ->
                do  json <- LBS.hGetContents handle
                    case Json.decode json of
                      Nothing -> return []
                      Just names -> return names

