{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Project.Gitlab
  ( getGitlabRepoSshUrls
  ) where

import qualified Data.ByteString.Lazy.Char8 as LazyChar8

import Control.Exception
import Data.Aeson
import Data.CaseInsensitive
import Data.Typeable
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Header
import Project.Exception

newtype Repo = Repo
  { sshUrl :: String
  } deriving (Show, Generic)

instance FromJSON Repo where
  parseJSON = withObject "Repo" $ \v -> Repo <$> v .: "ssh_url_to_repo"

type Token = String

type Url = String

getGitlabRepoSshUrls :: Token -> IO [String]
getGitlabRepoSshUrls token = do
  let url =
        "https://gitlab.com/api/v4/projects?owned=true&private_token=" ++ token
  manager <- newManager tlsManagerSettings
  fmap sshUrl <$> getRepos url manager

getRepos :: Url -> Manager -> IO [Repo]
getRepos url manager = do
  request <- withHeaders <$> parseUrlThrow url
  response <- httpLbs request manager
  case parseRepos . responseBody $ response of
    Right repos -> return repos
    Left err -> throwIO $ JsonException err
  where
    withHeaders request = request {requestHeaders = [(hUserAgent, "None")]}

parseRepos :: LazyChar8.ByteString -> Either String [Repo]
parseRepos = eitherDecode
