{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Project.Gitlab
  ( getGitlabRepoSshUrls,
  )
where

import Control.Exception
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Char8 as C
import Data.Typeable
import GHC.Generics
import Network.HTTP.Client
import Pipes
import qualified Pipes.Prelude as P
import Project.Config
import Project.HTTP
import Project.Logging

newtype Repo = Repo {sshUrl :: String}
  deriving (Show, Eq, Generic)

instance FromJSON Repo where
  parseJSON = withObject "Repo" $ \v -> Repo <$> v .: "ssh_url_to_repo"

newtype GitlabException = GitlabException String
  deriving (Show, Typeable)

instance Exception GitlabException

type Url = String

type Token = String

type PageNumber = Int

getGitlabRepoSshUrls :: Manager -> Config -> Producer String IO ()
getGitlabRepoSshUrls manager config = repos >-> P.map sshUrl
  where
    repos = getRepos manager (gitlabToken config)

getRepos :: Manager -> Token -> Producer Repo IO ()
getRepos man token = getRepos' 1
  where
    getRepos' page = do
      repos <- lift $ getPage man token page
      for (each repos) yield
      unless (null repos) $ getRepos' (page + 1)

getPage :: Manager -> Token -> PageNumber -> IO [Repo]
getPage man token page =
  debug (". " ++ url)
    *> (createRequest url token >>= getJSON GitlabException man)
    <* info ("✓ " ++ url)
  where
    url = "https://gitlab.com/api/v4/projects?owned=true&page=" ++ show page

createRequest :: Url -> Token -> IO Request
createRequest url token = parseUrlThrow url >>= \req ->
  pure
    req
      { requestHeaders = [("Private-Token", C.pack token)],
        responseTimeout = responseTimeoutMicro 60000000
      }
