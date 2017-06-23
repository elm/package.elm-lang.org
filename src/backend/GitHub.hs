{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module GitHub (Token, init, fetch) where


import Prelude hiding (init)
import Control.Exception (SomeException, catch)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http
import Network.HTTP.Types.Header (hAuthorization, hUserAgent)



-- TOKEN


data Token =
  Token
    { _manager :: Http.Manager
    , _token :: String
    }



-- INITIALIZE


init :: String -> IO Token
init githubToken =
  do  manager <- Http.newManager Http.tlsManagerSettings
      let token = Token manager githubToken
      response <- fetch token "/"
      case response of
        Left err ->
          error $ "Bad OAuth access token for GitHub.\n" ++ err

        Right _ ->
          return token



-- GITHUB REQUESTS


fetch :: Token -> String -> IO (Either String LBS.ByteString)
fetch (Token manager token) path =
  let
    recover e =
      return $ Left $ show (e :: SomeException)

    attempt =
      do  request <- Http.parseUrlThrow $ "https://api.github.com" ++ path
          response <- flip Http.httpLbs manager $ request
            { Http.requestHeaders =
                [ (hAuthorization, BS.pack ("token " ++ token))
                , (hUserAgent, "package.elm-lang.org")
                ]
            }
          return $ Right $ Http.responseBody response
  in
    attempt `catch` recover
