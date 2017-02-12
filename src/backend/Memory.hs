{-# OPTIONS_GHC -Wall #-}
module Memory
  ( Memory
  , init
  , getPackages
  , getHistory
  )
  where


import Prelude hiding (init)
import Control.Monad.Trans (liftIO)
import qualified Data.Map as Map
import Snap.Core (Snap)

import qualified Elm.Package as Pkg

import Memory.History (History)



-- MEMORY


data Memory =
  Memory
    { _getPackages :: IO Packages
    , _getHistory :: IO History
    }


type Packages = Map.Map Pkg.Name [Pkg.Version]



-- INITIALIZE


init :: IO Memory
init =
  error "TODO"



-- ACCESS


getPackages :: Memory -> Snap Packages
getPackages memory =
  liftIO (_getPackages memory)


getHistory :: Memory -> Int -> Snap History
getHistory memory index =
  liftIO (_getHistory memory)

