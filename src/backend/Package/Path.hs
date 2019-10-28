module Package.Path
	( releases
	, directory
	, removeDirectory
	)
	where


import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified Elm.Package as Pkg



-- PATHS


releases :: Pkg.Name -> FilePath
releases pkg =
  "packages" </> Pkg.toFilePath pkg </> "releases.json"


directory :: Pkg.Name -> Pkg.Version -> FilePath
directory name version =
  "packages" </> Pkg.toFilePath name </> Pkg.versionToString version


removeDirectory :: Pkg.Name -> Pkg.Version -> IO ()
removeDirectory name version =
  do  Dir.removeDirectoryRecursive $
  			"packages" </> Pkg.toFilePath name </> Pkg.versionToString version

  	  if version == Pkg.initial
  	  	then Dir.removeDirectoryRecursive ("packages" </> Pkg.toFilePath name)
  	  	else return ()
