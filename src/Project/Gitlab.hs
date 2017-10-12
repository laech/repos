{-# LANGUAGE DeriveGeneric #-}

module Project.Gitlab
  ( getGitlabRepoSshUrls
  ) where

import qualified Data.ByteString.Char8      as Char8
import qualified Data.ByteString.Lazy.Char8 as LazyChar8
import qualified Network.HTTP.Client        as Http

import           Control.Exception
import           Data.Aeson
import           Data.Typeable
import           GHC.Generics
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Project.Exception

newtype Repo = Repo
  { ssh_url_to_repo :: String
  } deriving (Show, Generic)

instance FromJSON Repo

type Token = String

type Url = String

getGitlabRepoSshUrls :: Token -> IO [String]
getGitlabRepoSshUrls token = do
  let url =
        "https://gitlab.com/api/v4/projects?owned=true&private_token=" ++ token
  manager <- Http.newManager tlsManagerSettings
  fmap ssh_url_to_repo <$> getRepos url manager

getRepos :: Url -> Http.Manager -> IO [Repo]
getRepos url manager = do
  request <- parseUrlThrow url
  response <- httpLbs request manager
  case parseRepos . responseBody $ response of
    Right repos -> return repos
    Left err    -> throwIO $ JsonException err

parseRepos :: LazyChar8.ByteString -> Either String [Repo]
parseRepos = eitherDecode
