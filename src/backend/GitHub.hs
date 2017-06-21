{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module GitHub (Token, init, fetch) where


import Prelude hiding (init)
import Control.Exception (SomeException, catch)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http



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
        Left _ ->
          error "Bad OAuth access token for GitHub"

        Right _ ->
          return token



-- GITHUB REQUESTS


fetch :: Token -> String -> IO (Either String BS.ByteString)
fetch (Token manager token) path =
  let
    recover e =
      return $ Left $ show (e :: SomeException)

    attempt =
      do  request <- Http.parseUrlThrow $ "https://api.github.com" ++ path ++ "?access_token=" ++ token
          response <- Http.httpLbs request manager
          return $ Right $ Http.responseBody response
  in
    attempt `catch` recover
