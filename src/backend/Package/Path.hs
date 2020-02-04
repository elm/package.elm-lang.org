module Package.Path
  ( releases
  , directory
  , removeDirectory
  )
  where


import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified Elm.Package as Pkg
import qualified Elm.Version as V



-- PATHS


releases :: Pkg.Name -> FilePath
releases pkg =
  "packages" </> Pkg.toFilePath pkg </> "releases.json"


directory :: Pkg.Name -> V.Version -> FilePath
directory pkg vsn =
  "packages" </> Pkg.toFilePath pkg </> V.toChars vsn


removeDirectory :: Pkg.Name -> V.Version -> IO ()
removeDirectory pkg vsn =
  do  Dir.removeDirectoryRecursive $
        "packages" </> Pkg.toFilePath pkg </> V.toChars vsn

      if vsn == V.one
        then Dir.removeDirectoryRecursive ("packages" </> Pkg.toFilePath pkg)
        else return ()
