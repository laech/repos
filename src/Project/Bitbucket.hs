{-# LANGUAGE OverloadedStrings #-}

module Project.Bitbucket
  ( getBitbucketRepoSshUrls
  ) where

import qualified Data.ByteString.Char8 as Char8
import qualified Data.HashMap.Strict as HM

import Data.Aeson
import Data.Aeson.Types
import Network.HTTP.Client
import Network.URI
import Project.Config
import System.IO.Unsafe
import System.Log.Logger

getBitbucketRepoSshUrls :: Manager -> Config -> IO [String]
getBitbucketRepoSshUrls manager config =
  repos manager (bitbucketUsername config) (bitbucketPassword config) url
  where
    url = "https://bitbucket.org/api/2.0/repositories/" ++ path
    path = escapeURIString isAllowedInURI (bitbucketUsername config)

repos :: Manager -> String -> String -> String -> IO [String]
repos manager user pass url = do
  debugM "Bitbucket" (". " ++ url)
  request <- basicAuthRequest url user pass
  response <- httpLbs request manager
  infoM "Bitbucket" ("✓ " ++ url)
  (urls, next) <- either fail return (parse response)
  (urls ++) <$> maybe (pure []) nextRepos next
  where
    parse response = eitherDecode (responseBody response) >>= parseEither page
    nextRepos = unsafeInterleaveIO . repos manager user pass

basicAuthRequest :: String -> String -> String -> IO Request
basicAuthRequest url user pass =
  applyBasicAuth (Char8.pack user) (Char8.pack pass) <$> parseUrlThrow url

page :: Value -> Parser ([String], Maybe String)
page =
  withObject "" $ \v -> do
    next <- v .:? "next"
    repos <- v .: "values" :: Parser [Object]
    links <- mapM (.: "links") repos
    clones <- filter isSsh . concat <$> mapM (.: "clone") links
    sshUrls <- mapM (.: "href") clones
    return (sshUrls, next)
  where
    isSsh clone = HM.lookup "name" clone == Just "ssh"