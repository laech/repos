{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Project.Gitlab
  ( getGitlabRepoUrls,
  )
where

import Control.Exception (Exception)
import Control.Monad (unless)
import Data.Aeson ((.:), FromJSON, parseJSON, withObject)
import qualified Data.ByteString.Char8 as C
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Network.HTTP.Client
  ( Manager,
    Request (requestHeaders, responseTimeout),
    parseUrlThrow,
    responseTimeoutMicro,
  )
import Pipes ((>->), Producer, each, for, lift, yield)
import qualified Pipes.Prelude as P
import Project.HTTP (getJSON)
import Project.Logging (debug, info)

newtype Repo = Repo {url :: String}
  deriving (Show, Eq, Generic)

instance FromJSON Repo where
  parseJSON = withObject "Repo" $ \v -> Repo <$> v .: "http_url_to_repo"

newtype GitlabException = GitlabException String
  deriving (Show, Typeable)

instance Exception GitlabException

type Url = String

type Token = String

type PageNumber = Int

getGitlabRepoUrls :: Manager -> Token -> Producer String IO ()
getGitlabRepoUrls manager token = repos >-> P.map url
  where
    repos = getRepos manager token

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
