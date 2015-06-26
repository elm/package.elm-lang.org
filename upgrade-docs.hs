{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Encoding as Text
import qualified Data.Text.Lazy.IO as Text
import qualified System.Directory as Dir
import qualified System.Environment as Env
import System.Exit (exitFailure)
import System.FilePath ((</>), takeExtension)
import System.IO (hPutStrLn, stderr)

import qualified Elm.Docs as Docs
import Elm.Utils ((|>))


main :: IO ()
main =
  do  allArgs <- Env.getArgs
      case allArgs of
        [pkg] ->
          do  fixAllDocs pkg
              fixModuleDocs pkg

        _ ->
          do  hPutStrLn stderr "Expecting a path to a particular version of a package"
              exitFailure


-- FIX documentation.json

fixAllDocs :: FilePath -> IO ()
fixAllDocs pkg =
  do  maybeValue <- Json.decode <$> BS.readFile (pkg </> "documentation.json")
      writeAsJson
        (pkg </> "new-documentation.json")
        (maybeValue :: Maybe [Docs.Documentation])


-- FIX docs/*.json

fixModuleDocs :: FilePath -> IO ()
fixModuleDocs pkg =
  do  moduleDocs <-
          filter (\path -> takeExtension path == ".json")
            <$> Dir.getDirectoryContents (pkg </> "docs")

      forM_ moduleDocs $ \file ->
        do  maybeValue <- Json.decode <$> BS.readFile (pkg </> "docs" </> file)
            Dir.createDirectoryIfMissing True (pkg </> "new-docs")
            writeAsJson
              (pkg </> "new-docs" </> file)
              (maybeValue :: Maybe Docs.Documentation)


-- WRITE JSON

writeAsJson :: (Json.ToJSON a) => FilePath -> Maybe a -> IO ()
writeAsJson file maybeValue =
      case maybeValue of
        Nothing ->
          do  hPutStrLn stderr "Problem reading JSON"
              exitFailure

        Just value ->
          Docs.prettyJson value
            |> Text.decodeUtf8
            |> Text.replace "\\u003e" ">"
            |> Text.writeFile file
