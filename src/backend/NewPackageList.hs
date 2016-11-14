{-# LANGUAGE OverloadedStrings #-}
module NewPackageList (newPackages, addIfNew) where

import qualified Data.Aeson as Json
import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Set as Set
import qualified System.Directory as Dir
import System.IO

import qualified Elm.Compiler as Compiler
import qualified Elm.Package.Constraint as C
import qualified Elm.Package.Description as Desc


newPackages :: String
newPackages =
    "new-packages-17.json"


privateNewPackages :: String
privateNewPackages =
    "new-packages.json"


addIfNew :: Desc.Description -> IO ()
addIfNew desc =
  case C.isSatisfied (Desc.elmVersion desc) Compiler.version of
    False ->
        return ()

    True ->
        do  let name = Desc.name desc
            exists <- Dir.doesFileExist privateNewPackages
            case exists of
              False ->
                  LBS.writeFile privateNewPackages (Json.encodePretty [name])

              True ->
                withBinaryFile privateNewPackages ReadMode $ \handle ->
                    do  json <- LBS.hGetContents handle
                        case Json.decode json of
                          Nothing ->
                              error "new-package.json is corrupted! do not modify them manually."

                          Just names ->
                              LBS.writeFile privateNewPackages (Json.encodePretty (Set.insert name names))
