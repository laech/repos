{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Project.Gitlab
  ( getGitlabRepoSshUrls
  ) where

import qualified Data.ByteString.Char8 as C
import qualified Pipes.Aeson as PA
import qualified Pipes.Prelude as P

import Control.Exception
import Control.Monad.Catch
import Control.Monad.IO.Class
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

getGitlabRepoSshUrls
  :: (MonadIO m, MonadThrow m)
  => Manager -> Config -> Producer String m ()
getGitlabRepoSshUrls manager config = repos >-> P.map sshUrl
  where
    url = "https://gitlab.com/api/v4/projects?owned=true"
    repos = getRepos manager url (gitlabToken config)

getRepos
  :: (MonadIO m, MonadThrow m)
  => Manager -> String -> String -> Producer Repo m ()
getRepos man url token = do
  liftIO $ debugM "Gitlab" (". " ++ url)
  req <- withToken token <$> parseUrlThrow url
  repos :: [Repo] <- getJSON req man
  liftIO $ infoM "Gitlab" ("✓ " ++ url)
  for (each repos) yield

getJSON
  :: (MonadIO m, FromJSON a)
  => Request -> Manager -> m a
getJSON req man =
  liftIO $
  withHTTP req man $ \resp ->
    evalStateT PA.decode (responseBody resp) >>= \case
      Nothing -> failed "end of input"
      Just (Left e) -> failed (show e)
      Just (Right r) -> pure r
  where
    failed = throwIO . GitlabException

withToken :: String -> Request -> Request
withToken token request =
  request {requestHeaders = [("Private-Token", C.pack token)]}
