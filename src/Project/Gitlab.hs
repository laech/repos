{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Project.Gitlab
  ( getGitlabRepoSshUrls
  ) where

import qualified Data.ByteString.Char8 as C

import Control.Monad
import Data.Aeson
import GHC.Generics
import Network.HTTP.Client
import Pipes
import Project.Config
import System.Log.Logger

newtype Repo = Repo
  { sshUrl :: String
  } deriving (Show, Generic)

instance FromJSON Repo where
  parseJSON = withObject "Repo" $ \v -> Repo <$> v .: "ssh_url_to_repo"

getGitlabRepoSshUrls :: Manager -> Config -> Producer String IO ()
getGitlabRepoSshUrls manager config =
  for (repos manager $ gitlabToken config) $ yield . sshUrl

repos :: Manager -> String -> Producer Repo IO ()
repos manager token = do
  let url = "https://gitlab.com/api/v4/projects?owned=true"
  lift $ debugM "Gitlab" (". " ++ url)
  request <- withToken token <$> parseUrlThrow url
  repos' <- lift $ readContent manager url token
  lift $ infoM "Gitlab" ("✓ " ++ url)
  for (each repos') yield

readContent :: Manager -> String -> String -> IO [Repo]
readContent manager url token = do
  request <- withToken token <$> parseUrlThrow url
  withResponse request manager $ \response -> do
    content <- responseBody response
    result <- either fail pure $ eitherDecodeStrict content
    pure result

withToken :: String -> Request -> Request
withToken token request =
  request {requestHeaders = [("Private-Token", C.pack token)]}
