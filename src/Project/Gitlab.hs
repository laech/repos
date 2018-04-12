{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Project.Gitlab
  ( getGitlabRepoSshUrls
  ) where

import qualified Data.ByteString.Char8 as C
import qualified Pipes.Aeson as PA
import qualified Pipes.Prelude as P

import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Typeable
import GHC.Generics
import Network.HTTP.Client
import Pipes
import Pipes.HTTP
import Pipes.Parse
import Project.Config
import System.Log.Logger

newtype Repo = Repo
  { sshUrl :: String
  } deriving (Show, Eq, Generic)

instance FromJSON Repo where
  parseJSON = withObject "Repo" $ \v -> Repo <$> v .: "ssh_url_to_repo"

newtype GitlabException =
  GitlabException String
  deriving (Show, Typeable)

instance Exception GitlabException

getGitlabRepoSshUrls :: Manager -> Config -> Producer String IO ()
getGitlabRepoSshUrls manager config =
  getRepos manager (gitlabToken config) >-> P.map sshUrl

getRepos :: Manager -> String -> Producer Repo IO ()
getRepos manager token = do
  let url = "https://gitlab.com/api/v4/projects?owned=true"
  lift $ debugM "Gitlab" (". " ++ url)
  request <- withToken token <$> parseUrlThrow url
  repos <- lift $ readContent manager url token
  lift $ infoM "Gitlab" ("✓ " ++ url)
  for (each repos) yield

readContent :: Manager -> String -> String -> IO [Repo]
readContent manager url token = do
  request <- withToken token <$> parseUrlThrow url
  withHTTP request manager $ \response ->
    evalStateT PA.decode (responseBody response) >>=
    maybe (failed "end of input") pure >>=
    either (failed . show) pure
  where
    failed = throwIO . GitlabException

withToken :: String -> Request -> Request
withToken token request =
  request {requestHeaders = [("Private-Token", C.pack token)]}
