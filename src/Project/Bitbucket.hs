{-# LANGUAGE OverloadedStrings #-}

module Project.Bitbucket
  ( getBitbucketRepoSshUrls
  ) where

import qualified Data.ByteString.Char8 as C

import Data.Aeson
import Data.Aeson.Types
import Data.HashMap.Strict ((!))
import Network.HTTP.Client
import Network.URI
import Pipes
import Project.Config
import System.Log.Logger

getBitbucketRepoSshUrls :: Manager -> Config -> Producer String IO ()
getBitbucketRepoSshUrls manager config =
  repos manager (bitbucketUsername config) (bitbucketPassword config) url
  where
    url = "https://bitbucket.org/api/2.0/repositories/" ++ path
    path = escapeURIString isAllowedInURI (bitbucketUsername config)

repos :: Manager -> String -> String -> String -> Producer String IO ()
repos manager user pass url = do
  result <- lift $ readPage manager url user pass
  (urls, nextUrl) <- either fail pure result
  for (each urls) yield
  maybe (pure ()) next nextUrl
  where
    next = repos manager user pass

type Page = ([String], Maybe String)

readPage :: Manager -> String -> String -> String -> IO (Either String Page)
readPage manager url user pass = do
  debugM "Bitbucket" (". " ++ url)
  request <- basicAuthRequest url user pass
  withResponse request manager (fmap parse . responseBody) <*
    infoM "Bitbucket" ("✓ " ++ url)
  where
    parse content = eitherDecodeStrict content >>= parseEither parsePage

basicAuthRequest :: String -> String -> String -> IO Request
basicAuthRequest url user pass =
  applyBasicAuth (C.pack user) (C.pack pass) <$> parseUrlThrow url

parsePage :: Value -> Parser ([String], Maybe String)
parsePage =
  withObject "" $ \v -> do
    next <- v .:? "next"
    repos <- v .: "values" :: Parser [Object]
    links <- mapM (.: "links") repos
    clones <- concat <$> mapM (.: "clone") links
    sshUrls <- mapM (.: "href") $ filter isSsh clones
    pure (sshUrls, next)
  where
    isSsh clone = clone ! "name" == "ssh"
