{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Project.Bitbucket
  ( getBitbucketRepoSshUrls
  ) where

import qualified Data.ByteString.Char8 as C
import qualified Pipes.Parse as P

import Control.Exception
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

getBitbucketRepoSshUrls :: Manager -> Config -> Producer String IO ()
getBitbucketRepoSshUrls manager config =
  getRepos manager (bitbucketUsername config) (bitbucketPassword config) url
  where
    url = "https://bitbucket.org/api/2.0/repositories/" ++ path
    path = escapeURIString isAllowedInURI (bitbucketUsername config)

getRepos :: Manager -> String -> String -> String -> Producer String IO ()
getRepos manager user pass url = do
  (Page urls nextUrl) <- lift $ getPage manager url user pass
  for (each urls) yield
  maybe (pure ()) next nextUrl
  where
    next = getRepos manager user pass

getPage :: Manager -> String -> String -> String -> IO Page
getPage manager url user pass = do
  debugM "Bitbucket" (". " ++ url)
  request <- basicAuthRequest url user pass
  withHTTP request manager $ \response -> do
    result <- P.evalStateT PA.decode (responseBody response)
    case result of
      Nothing -> failed "end of input"
      Just (Left e) -> failed (show e)
      Just (Right page) -> page <$ infoM "Bitbucket" ("✓ " ++ url)
  where
    failed = throwIO . BitbucketException

basicAuthRequest :: String -> String -> String -> IO Request
basicAuthRequest url user pass =
  applyBasicAuth (C.pack user) (C.pack pass) <$> parseUrlThrow url
