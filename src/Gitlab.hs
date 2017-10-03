{-# LANGUAGE DeriveGeneric #-}

module Gitlab
  ( getGitlabRepoSshUrls
  ) where

import qualified Data.ByteString.Char8      as Char8
import qualified Data.ByteString.Lazy.Char8 as LazyChar8
import qualified Network.HTTP.Client        as Http

import           Data.Aeson
import           GHC.Generics
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

newtype Repo = Repo
  { ssh_url_to_repo :: String
  } deriving (Show, Generic)

instance FromJSON Repo

type Token = String

type Url = String

getGitlabRepoSshUrls :: Token -> IO (Either String [String])
getGitlabRepoSshUrls token =
  let url =
        "https://gitlab.com/api/v4/projects?owned=true&private_token=" ++ token
      manager = Http.newManager tlsManagerSettings
  in (fmap . fmap) ssh_url_to_repo <$> (getRepos url =<< manager)

getRepos :: Url -> Http.Manager -> IO (Either String [Repo])
getRepos url manager = do
  request <- parseUrlThrow url
  response <- httpLbs request manager
  pure . parseRepos . responseBody $ response

parseRepos :: LazyChar8.ByteString -> Either String [Repo]
parseRepos = eitherDecode
