{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Project.Gitlab
  ( getGitlabRepoSshUrls
  ) where

import qualified Data.Aeson.Types as Json
import qualified Data.ByteString.Char8 as Char8

import Data.Aeson
import GHC.Generics
import Network.HTTP.Client

newtype Repo = Repo
  { sshUrl :: String
  } deriving (Show, Generic)

instance FromJSON Repo where
  parseJSON = withObject "Repo" $ \v -> Repo <$> v .: "ssh_url_to_repo"

getGitlabRepoSshUrls :: Manager -> String -> IO [String]
getGitlabRepoSshUrls manager token = map sshUrl <$> repos manager token

repos :: Manager -> String -> IO [Repo]
repos manager token = do
  let url = "https://gitlab.com/api/v4/projects?owned=true"
  request <- withToken token <$> parseUrlThrow url
  response <- httpLbs request manager
  either fail return (eitherDecode . responseBody $ response)

withToken :: String -> Request -> Request
withToken token request =
  request {requestHeaders = [("Private-Token", Char8.pack token)]}