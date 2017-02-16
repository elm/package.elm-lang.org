{-# OPTIONS_GHC -Wall #-}
module Memory
  ( Memory
  , init
  , getPackages
  , getHistory
  , addPackage
  )
  where


import Prelude hiding (init)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad.Trans (liftIO)
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import Data.Text (Text)
import Snap.Core (Snap)
import qualified System.Directory as Dir

import Elm.Package (Name, Version)
import qualified Elm.Package as Pkg

import Memory.History (History)
import qualified Memory.History as History
import qualified Memory.Timeline as Timeline



-- MEMORY


data Memory =
  Memory
    { _getPackages :: IO Packages
    , _getHistory :: IO History
    , _addPackage :: Name -> Version -> IO ()
    }


type Packages = Map.Map Name [Version]



-- SNAP


getPackages :: Memory -> Snap Packages
getPackages memory =
  liftIO (_getPackages memory)


getHistory :: Memory -> Snap History
getHistory memory =
  liftIO (_getHistory memory)


addPackage :: Memory -> Name -> Version -> Snap ()
addPackage memory name version =
  liftIO (_addPackage memory name version)



-- INIT


init :: IO Memory
init =
  do  timeline <- Timeline.read

      let history = History.fromTimeline timeline
      let packages = History.toDict history
      replaceAllPackages packages

      chan <- newChan

      _ <- forkIO $ loop chan history packages

      return $
        Memory
          { _getPackages =
              do  mvar <- newEmptyMVar
                  writeChan chan (GetPackages mvar)
                  takeMVar mvar
          , _getHistory =
              do  mvar <- newEmptyMVar
                  writeChan chan (GetHistory mvar)
                  takeMVar mvar
          , _addPackage =
              \name version -> writeChan chan (AddEvent name version)
          }



-- WORKER


data Msg
  = GetPackages (MVar Packages)
  | GetHistory (MVar History)
  | AddEvent Name Version


loop :: Chan Msg -> History -> Packages -> IO ()
loop chan history packages =
  do  msg <- readChan chan
      case msg of
        GetPackages mvar ->
          do  putMVar mvar packages
              loop chan history packages

        GetHistory mvar ->
          do  putMVar mvar history
              loop chan history packages

        AddEvent name version ->
          do  let newAll = Map.insertWith (++) name [version] packages
              replaceAllPackages newAll
              loop chan (History.add name version history) newAll



-- REPLACE JSON


temp :: FilePath
temp =
  "all-packages-temp.json"


replaceAllPackages :: Map.Map Name [Version] -> IO ()
replaceAllPackages allPackages =
  do  BS.writeFile temp $ Json.encode $ Json.object $
        map toPair $ Map.toList allPackages

      Dir.renameFile temp "all-packages.json"


toPair :: (Name, [Version]) -> (Text, Json.Value)
toPair (name, versions) =
  ( Pkg.toText name, Json.toJSON versions )
