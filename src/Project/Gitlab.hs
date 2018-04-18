{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Project.Gitlab
  ( getGitlabRepoSshUrls
  ) where

import qualified Data.ByteString.Char8 as C
import qualified Pipes.Prelude as P

import Control.Exception
import Data.Aeson
import Data.Typeable
import GHC.Generics
import Network.HTTP.Client
import Pipes
import Pipes.Core
import Project.Config
import Project.HTTP
import Project.Logging

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
getGitlabRepoSshUrls manager config = repos >-> P.map sshUrl
  where
    url = "https://gitlab.com/api/v4/projects?owned=true"
    repos = getRepos manager url (gitlabToken config)

getRepos :: Manager -> String -> String -> Producer Repo IO ()
getRepos man url token = each =<< lift (getPage man url token) //> yield

getPage :: Manager -> String -> String -> IO [Repo]
getPage man url token =
  debug (". " ++ url) *>
  (createRequest url token >>= getJSON GitlabException man) <*
  info ("✓ " ++ url)

createRequest :: String -> String -> IO Request
createRequest url token =
  parseUrlThrow url >>= \req ->
    pure req {requestHeaders = [("Private-Token", C.pack token)]}
