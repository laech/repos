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
  content <- lift $ readContent manager url user pass
  (urls, nextUrl) <- either fail pure (parse content)
  for (each urls) yield
  maybe (pure ()) next nextUrl
  where
    parse content = eitherDecode content >>= parseEither page
    next = repos manager user pass

readContent manager url user pass = do
  debugM "Bitbucket" (". " ++ url)
  request <- basicAuthRequest url user pass
  response <- httpLbs request manager
  infoM "Bitbucket" ("✓ " ++ url)
  pure $ responseBody response

basicAuthRequest :: String -> String -> String -> IO Request
basicAuthRequest url user pass =
  applyBasicAuth (C.pack user) (C.pack pass) <$> parseUrlThrow url

page :: Value -> Parser ([String], Maybe String)
page =
  withObject "" $ \v -> do
    next <- v .:? "next"
    repos <- v .: "values" :: Parser [Object]
    links <- mapM (.: "links") repos
    clones <- concat <$> mapM (.: "clone") links
    sshUrls <- mapM (.: "href") $ filter isSsh clones
    pure (sshUrls, next)
  where
    isSsh clone = clone ! "name" == "ssh"
