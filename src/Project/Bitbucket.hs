{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Project.Bitbucket
  ( getBitbucketRepoSshUrls
  ) where

import qualified Data.ByteString.Char8 as C

import Control.Applicative
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
import Project.HTTP
import Project.Logging

data Page = Page
  { sshUrls :: [String]
  , nextUrl :: Maybe String
  } deriving (Show, Eq, Generic)

instance FromJSON Page where
  parseJSON = withObject "" $ \v -> Page <$> sshUrls v <*> next v
    where
      next v = v .:? "next"
      sshUrls v =
        (v .: "values" :: Parser [Object]) >>= --
        mapM (.: "links") >>= --
        fmap concat . mapM (.: "clone") >>= --
        mapM (.: "href") . filter (\x -> x ! "name" == "ssh")

newtype BitbucketException =
  BitbucketException String
  deriving (Show, Typeable)

instance Exception BitbucketException

getBitbucketRepoSshUrls :: Manager -> Config -> Producer String IO ()
getBitbucketRepoSshUrls manager config = getRepos manager user pass url
  where
    url = "https://bitbucket.org/api/2.0/repositories/" ++ path
    user = bitbucketUsername config
    pass = bitbucketPassword config
    path = escapeURIString isAllowedInURI (bitbucketUsername config)

getRepos :: Manager -> String -> String -> String -> Producer String IO ()
getRepos man user pass url = lift (getPage man url user pass) >>= process
  where
    next = getRepos man user pass
    process (Page urls nextUrl) =
      for (each urls) yield *> maybe (pure ()) next nextUrl

getPage :: Manager -> String -> String -> String -> IO Page
getPage man url user pass =
  debug (". " ++ url) >>
  (basicAuth url user pass >>= getJSON BitbucketException man) <*
  info ("✓ " ++ url)

basicAuth :: String -> String -> String -> IO Request
basicAuth url user pass =
  applyBasicAuth (C.pack user) (C.pack pass) <$> parseUrlThrow url
