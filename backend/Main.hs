{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module Main where

import Control.Applicative
import Control.Monad.Error
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as Map
import GHC.Conc
import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe
import System.Console.CmdArgs
import System.Directory

import qualified Routes as Route


data Flags = Flags
    { port :: Int
    }
    deriving (Data,Typeable,Show,Eq)


flags :: Flags
flags = Flags
    { port = 8000 &= help "set the port of the server"
    }


-- | Set up the server.
main :: IO ()
main =
  do  setNumCapabilities =<< getNumProcessors
      setupLogging
      cargs <- cmdArgs flags
      httpServe (setPort (port cargs) defaultConfig) $
          ifTop (serveFile "public/Home.html")
          <|> route [ ("catalog"  , Route.catalog)
                    , ("versions" , Route.versions)
                    , ("register" , Route.register)
                    , ("documentation" , Route.documentation)
                    , ("assets", serveDirectoryWith directoryConfig "assets")
                    ]
          <|> serveDirectoryWith directoryConfig "public"
          <|> do modifyResponse $ setResponseStatus 404 "Not found"
                 serveFile "public/Error404.html"


setupLogging :: IO ()
setupLogging =
  do  createDirectoryIfMissing True "log"
      createIfMissing "log/access.log"
      createIfMissing "log/error.log"
  where
    createIfMissing path =
      do  exists <- doesFileExist path
          when (not exists) $ BS.writeFile path ""


directoryConfig :: MonadSnap m => DirectoryConfig m
directoryConfig =
    fancyDirectoryConfig
    { indexGenerator = defaultIndexGenerator defaultMimeTypes indexStyle
    , mimeTypes =
        Map.insert ".elm" "text/plain" $
        Map.insert ".ico" "image/x-icon" $ defaultMimeTypes
    }


indexStyle :: BS.ByteString
indexStyle =
    "body { margin:0; font-family:sans-serif; background:rgb(245,245,245);\
    \       font-family: calibri, verdana, helvetica, arial; }\
    \div.header { padding: 40px 50px; font-size: 24px; }\
    \div.content { padding: 0 40px }\
    \div.footer { display:none; }\
    \table { width:100%; border-collapse:collapse; }\
    \td { padding: 6px 10px; }\
    \tr:nth-child(odd) { background:rgb(216,221,225); }\
    \td { font-family:monospace }\
    \th { background:rgb(90,99,120); color:white; text-align:left;\
    \     padding:10px; font-weight:normal; }"
