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
import Control.Monad
import Data.Aeson
import Data.Typeable
import GHC.Generics
import Network.HTTP.Client
import Pipes
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
    repos = getRepos manager (gitlabToken config)

getRepos :: Manager -> String -> Producer Repo IO ()
getRepos man token = getRepos' 1
  where
    getRepos' page = do
      repos <- lift $ getPage man token page
      for (each repos) yield
      unless (null repos) $ getRepos' (page + 1)

getPage :: Manager -> String -> Int -> IO [Repo]
getPage man token page =
  debug (". " ++ url) *>
  (createRequest url token >>= getJSON GitlabException man) <*
  info ("✓ " ++ url)
  where
    url = "https://gitlab.com/api/v4/projects?owned=true&page=" ++ show page

createRequest :: String -> String -> IO Request
createRequest url token =
  parseUrlThrow url >>= \req ->
    pure req {requestHeaders = [("Private-Token", C.pack token)]}
