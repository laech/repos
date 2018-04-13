{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Project.Bitbucket
  ( getBitbucketRepoSshUrls
  ) where

import qualified Data.ByteString.Char8 as C
import qualified Pipes.Parse as P

import Control.Exception
import Control.Monad.Catch
import Data.Aeson
import Data.Aeson.Types
import Data.HashMap.Strict ((!))
import Data.Typeable
import GHC.Generics
import Network.HTTP.Client
import Network.URI
import Pipes
import Pipes.Aeson as PA
import Pipes.HTTP
import Project.Config
import Project.HTTP
import System.Log.Logger

data Page = Page
  { sshUrls :: [String]
  , nextUrl :: Maybe String
  } deriving (Show, Eq, Generic)

instance FromJSON Page where
  parseJSON =
    withObject "" $ \v -> do
      next <- v .:? "next"
      repos <- v .: "values" :: Parser [Object]
      links <- mapM (.: "links") repos
      clones <- concat <$> mapM (.: "clone") links
      sshUrls <- mapM (.: "href") $ filter isSsh clones
      pure $ Page sshUrls next
    where
      isSsh clone = clone ! "name" == "ssh"

newtype BitbucketException =
  BitbucketException String
  deriving (Show, Typeable)

instance Exception BitbucketException

getBitbucketRepoSshUrls
  :: (MonadIO m, MonadThrow m)
  => Manager -> Config -> Producer String m ()
getBitbucketRepoSshUrls manager config = getRepos manager user pass url
  where
    url = "https://bitbucket.org/api/2.0/repositories/" ++ path
    user = bitbucketUsername config
    pass = bitbucketPassword config
    path = escapeURIString isAllowedInURI (bitbucketUsername config)

getRepos
  :: (MonadIO m, MonadThrow m)
  => Manager -> String -> String -> String -> Producer String m ()
getRepos man user pass url = do
  (Page urls nextUrl) <- getPage man url user pass
  for (each urls) yield
  maybe (pure ()) next nextUrl
  where
    next = getRepos man user pass

getPage
  :: (MonadIO m, MonadThrow m)
  => Manager -> String -> String -> String -> m Page
getPage man url user pass = do
  req <- basicAuthRequest url user pass
  liftIO $
    debugM "Bitbucket" (". " ++ url) >> --
    getJSON req man BitbucketException <* --
    infoM "Bitbucket" ("✓ " ++ url)

basicAuthRequest
  :: MonadThrow m
  => String -> String -> String -> m Request
basicAuthRequest url user pass =
  applyBasicAuth (C.pack user) (C.pack pass) <$> parseUrlThrow url
