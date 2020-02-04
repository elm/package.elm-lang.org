{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Legacy
  ( Version
  , getElmVersion
  , serve
  )
  where


import Control.Monad.Trans (liftIO)
import qualified Data.Binary as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS_UTF8
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Snap.Core as S
import Snap.Core (Snap, getParam, pass, writeBS, writeLBS)
import Snap.Util.FileServe (serveFile)
import qualified System.Directory as Dir
import System.FilePath ((</>))
import System.IO (IOMode(ReadMode), withBinaryFile)
import qualified Text.Read as Read

import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified Json.Decode as D
import qualified Parse.Primitives as P
import Parse.Primitives (Row, Col)

import qualified Server.Router as Router
import Server.Router ((==>), s)



-- GET ELM VERSION


data Version
  = Legacy_18
  | Legacy_17
  | Legacy_16
  | Legacy_OLD


getElmVersion :: Snap Version
getElmVersion =
  do  maybeVersion <- getParam "elm-package-version"
      case maybeVersion of
        Nothing       -> pass
        Just "0.18"   -> return Legacy_18
        Just "0.17.1" -> return Legacy_17
        Just "0.17"   -> return Legacy_17
        Just "0.16"   -> return Legacy_16
        Just _        -> return Legacy_OLD



-- SERVE


serve :: Version -> Snap ()
serve version =
  Router.serve $ Router.oneOf $
    [ s "versions" ==> serveVersions
    , s "permissions" ==> serveEffectManagerPermissions
    , s "description" ==> servePackageFile "elm-package.json"
    , s "documentation" ==> servePackageFile "documentation.json"
    , s "all-packages" ==> serveAllPackages version
    , s "new-packages" ==> serveFile ("legacy" </> "new-packages.json")
    ]



-- SERVE /versions


serveVersions :: Snap ()
serveVersions =
  do  name <- getParameter "name" pkgFromByteString
      versions <- liftIO (readVersions name)
      writeLBS (Binary.encode versions)


data Summary =
  Summary
    { _name :: Pkg.Name
    , _versions :: [V.Version]
    }


readVersions :: Pkg.Name -> IO (Maybe [V.Version])
readVersions pkg =
  withBinaryFile ("legacy" </> "all-packages.18.json") ReadMode $ \handle ->
    do  bytes <- BS.hGetContents handle
        case D.fromByteString (D.list summaryDecoder) bytes of
          Left _ ->
            error "corrupt JSON in legacy/all-packages.18.json"

          Right summaries ->
            return $ fmap _versions $ List.find ((==) pkg . _name) summaries


summaryDecoder :: D.Decoder (Row,Col) Summary
summaryDecoder =
  Summary
    <$> D.field "name" Pkg.decoder
    <*> D.field "versions" (D.list V.decoder)



-- SERVE /permissions


serveEffectManagerPermissions :: Snap ()
serveEffectManagerPermissions =
  do  name <- getParameter "name" Just
      writeLBS (Binary.encode (Set.member name effectWhitelist))


effectWhitelist :: Set.Set BS.ByteString
effectWhitelist =
  Set.fromList
    [ "elm-lang/animation-frame"
    , "elm-lang/core"
    , "elm-lang/geolocation"
    , "elm-lang/http"
    , "elm-lang/keyboard"
    , "elm-lang/mouse"
    , "elm-lang/navigation"
    , "elm-lang/page-visibility"
    , "elm-lang/websocket"
    , "elm-lang/window"
    ]



-- SERVE /description AND /documentation


servePackageFile :: FilePath -> Snap ()
servePackageFile fileName =
  do  pkg <- getParameter "name" pkgFromByteString
      vsn <- getParameter "version" vsnFromByteString

      let path =
            "legacy" </> "packages"
            </> Pkg.toFilePath pkg </> V.toChars vsn </> fileName

      exists <- liftIO $ Dir.doesFileExist path

      if exists
        then serveFile path
        else httpError 404 "That package and version is not registered."



-- SERVE /all-packages


serveAllPackages :: Version -> Snap ()
serveAllPackages version =
  do  rawTime <- getParam "since"
      case Read.readMaybe =<< fmap BS_UTF8.toString rawTime of
        Nothing ->
          serveFile (toAllPackagesPath version)

        Just remoteTime ->
          do  let path = toAllPackagesPath version
              localTime <- liftIO (Dir.getModificationTime path)
              if remoteTime < localTime
                then serveFile path
                else writeBS "null"


toAllPackagesPath :: Version -> FilePath
toAllPackagesPath version =
  case version of
    Legacy_18  -> "legacy" </> "all-packages.18.json"
    Legacy_17  -> "legacy" </> "all-packages.17.json"
    Legacy_16  -> "legacy" </> "all-packages.16.json"
    Legacy_OLD -> "legacy" </> "all-packages.15.json"



-- HELPERS


getParameter :: BS.ByteString -> (BS.ByteString -> Maybe a) -> Snap a
getParameter paramName fromByteString =
  do  maybeValue <- getParam paramName
      case maybeValue of
        Nothing ->
          httpError 400 $ BS.concat [ "could not find parameter named ", paramName ]

        Just bytes ->
          case fromByteString bytes of
            Nothing ->
              httpError 400 $ BS.concat [ "problem with parameter \"", paramName, "\": ", bytes ]

            Just value ->
              return value


httpError :: Int -> BS.ByteString -> Snap a
httpError code msg =
  do  S.modifyResponse $ S.setResponseCode code
      writeBS msg
      S.finishWith =<< S.getResponse


pkgFromByteString :: BS.ByteString -> Maybe Pkg.Name
pkgFromByteString bytes =
  case P.fromByteString Pkg.parser (,) bytes of
    Right pkg -> Just pkg
    Left _    -> Nothing


vsnFromByteString :: BS.ByteString -> Maybe V.Version
vsnFromByteString bytes =
  case P.fromByteString V.parser (,) bytes of
    Right vsn -> Just vsn
    Left _    -> Nothing
