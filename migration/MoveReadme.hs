{-# OPTIONS_GHC -Wall #-}
module MoveReadme (move) where

import Control.Monad.Trans (liftIO)
import qualified System.Directory as Dir
import System.FilePath ((</>))

import qualified Crawl
import qualified Elm.Package as Pkg
import qualified Task



-- MOVE


move :: Pkg.Name -> Pkg.Version -> Task.Transaction ()
move pkg version =
  liftIO $
    Dir.copyFileWithMetadata
      (Crawl.oldDir pkg version </> "README.md")
      (Crawl.newDir pkg version </> "README.md")
