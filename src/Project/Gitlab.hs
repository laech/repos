{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Project.Gitlab
  ( getGitlabRepoSshUrls
  ) where

import qualified Data.ByteString.Char8 as C
import qualified Pipes.Aeson as PA
import qualified Pipes.Prelude as P

import Control.Exception
import Control.Monad.Base
import Control.Monad.Trans.Control
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

getGitlabRepoSshUrls ::
     MonadBase IO m => Manager -> Config -> Producer String m ()
getGitlabRepoSshUrls manager config = repos >-> P.map sshUrl
  where
    url = "https://gitlab.com/api/v4/projects?owned=true"
    repos = getRepos manager url (gitlabToken config)

getRepos :: MonadBase IO m => Manager -> String -> String -> Producer Repo m ()
getRepos man url token = do
  req <- lift . liftBase $ withToken token <$> parseUrlThrow url
  lift . debug "Gitlab" $ ". " ++ url
  repos :: [Repo] <- lift $ getJSON req man GitlabException
  lift . info "Gitlab" $ "✓ " ++ url
  for (each repos) yield

withToken :: String -> Request -> Request
withToken token request =
  request {requestHeaders = [("Private-Token", C.pack token)]}
