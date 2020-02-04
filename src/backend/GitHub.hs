{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module GitHub
  ( Token
  , init
  , fetchPath
  )
  where


import Prelude hiding (init)
import Control.Exception (SomeException, catch)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http
import qualified Network.HTTP.Types.Header as Http (hAcceptEncoding, hAuthorization, hUserAgent)



-- TOKEN


data Token =
  Token
    { _manager :: Http.Manager
    , _token :: BS.ByteString
    }



-- INITIALIZE


init :: String -> IO Token
init githubToken =
  do  manager <- Http.newManager Http.tlsManagerSettings
      let token = Token manager (BS.pack ("token " ++ githubToken))
      response <- fetchPath token "/"
      case response of
        Left err ->
          error $ "Bad OAuth access token for GitHub.\n" ++ err

        Right _ ->
          return token



-- GITHUB REQUESTS


fetchPath :: Token -> String -> IO (Either String LBS.ByteString)
fetchPath (Token manager token) path =
  let
    attempt =
      do  request <- Http.parseUrlThrow $ "https://api.github.com" ++ path
          response <- Http.httpLbs (addGitHubHeaders token request) manager
          return $ Right $ Http.responseBody response
  in
  attempt `catch` recover


recover :: SomeException -> IO (Either String a)
recover e =
  return $ Left $ show e


addGitHubHeaders :: BS.ByteString -> Http.Request -> Http.Request
addGitHubHeaders token request =
  request
    { Http.requestHeaders =
        [ (Http.hAuthorization, token)
        , (Http.hUserAgent, "package.elm-lang.org")
        , (Http.hAcceptEncoding, "gzip")
        ]
    }
