{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Project.Gitlab
  ( getGitlabRepoSshUrls
  ) where

import qualified Data.ByteString.Char8 as C

import Data.Aeson
import GHC.Generics
import Network.HTTP.Client
import Project.Config
import Pipes
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
  content <- lift $ readContent manager url token
  items <- either fail pure (eitherDecode content :: Either String [Repo])
  for (each items) yield

readContent manager url token = do
  debugM "Gitlab" (". " ++ url)
  request <- withToken token <$> parseUrlThrow url
  response <- httpLbs request manager
  infoM "Gitlab" ("✓ " ++ url)
  pure $ responseBody response

withToken :: String -> Request -> Request
withToken token request =
  request {requestHeaders = [("Private-Token", C.pack token)]}
