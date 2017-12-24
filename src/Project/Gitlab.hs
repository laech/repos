{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Project.Gitlab
  ( getGitlabRepoSshUrls
  ) where

import qualified Data.ByteString.Lazy.Char8 as LazyChar8

import Control.Exception
import Data.Aeson
import Data.CaseInsensitive
import Data.Typeable
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Types.Header
import Network.URI

newtype Repo = Repo
  { sshUrl :: String
  } deriving (Show, Generic)

instance FromJSON Repo where
  parseJSON = withObject "Repo" $ \v -> Repo <$> v .: "ssh_url_to_repo"

type Token = String

type Url = String

getGitlabRepoSshUrls :: Manager -> Token -> IO [String]
getGitlabRepoSshUrls manager token =
  fmap sshUrl <$>
  getRepos
    manager
    ("https://gitlab.com/api/v4/projects?owned=true&private_token=" ++
     escapeURIString isAllowedInURI token)

getRepos :: Manager -> Url -> IO [Repo]
getRepos manager url = do
  request <- withHeaders <$> parseUrlThrow url
  response <- httpLbs request manager
  either fail return (eitherDecode . responseBody $ response)
  where
    withHeaders request = request {requestHeaders = [(hUserAgent, "None")]}