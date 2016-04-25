{-# LANGUAGE OverloadedStrings #-}
module Whitelists (checkNative, checkEffect) where

import Prelude hiding (read)
import Data.Aeson as Json
import qualified Data.ByteString.Lazy as LBS
import qualified System.Directory as Dir
import System.IO

import qualified Elm.Package as Pkg



-- PUBLIC API


checkNative :: Pkg.Name -> IO Bool
checkNative name =
  check "native-whitelist.json" name


checkEffect :: Pkg.Name -> IO Bool
checkEffect name =
  check "effect-whitelist.json" name



-- READ JSON


check :: FilePath -> Pkg.Name -> IO Bool
check path name =
  do  exists <- Dir.doesFileExist path
      case exists of
        False ->
          return False

        True ->
          withBinaryFile path ReadMode $ \handle ->
            do  json <- LBS.hGetContents handle
                let maybePkgs = (Json.decode json :: Maybe [Pkg.Name])
                let isWhitelisted = maybe False (elem name) maybePkgs
                isWhitelisted `seq` return isWhitelisted
