{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Project.Bitbucket
  ( getBitbucketRepoSshUrls
  ) where

import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy.Char8 as LazyChar8

import Control.Exception
import Control.Monad.Loops
import Data.Aeson
import Data.List
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Types.URI
import Network.URI

data Page = Page
  { values :: [Repository]
  , next :: Maybe String
  } deriving (Generic, Show, FromJSON)

newtype Repository = Repository
  { links :: Links
  } deriving (Generic, Show, FromJSON)

newtype Links = Links
  { clone :: [Clone]
  } deriving (Generic, Show, FromJSON)

data Clone = Clone
  { name :: String
  , href :: Url
  } deriving (Generic, Show, FromJSON)

type Url = String

type User = String

type Pass = String

getBitbucketRepoSshUrls :: Manager -> String -> String -> IO [String]
getBitbucketRepoSshUrls manager username password =
  downloadSshUrls
    manager
    username
    password
    ("https://bitbucket.org/api/2.0/repositories/" ++
     escapeURIString isAllowedInURI username)

downloadSshUrls :: Manager -> User -> Pass -> Url -> IO [String]
downloadSshUrls manager user pass url = do
  pages <- downloadPages manager user pass url
  return $ concatMap extractSshUrls pages

downloadPages :: Manager -> User -> Pass -> Url -> IO [Page]
downloadPages manager user pass url = unfoldrM download (Just url)
  where
    download :: Maybe Url -> IO (Maybe (Page, Maybe Url))
    download Nothing = return Nothing
    download (Just url) = do
      page <- downloadPage url user pass manager
      return $ Just (page, next page)

basicAuthRequest :: Url -> User -> Pass -> IO Request
basicAuthRequest url user pass =
  applyBasicAuth (Char8.pack user) (Char8.pack pass) <$> parseUrlThrow url

downloadPage :: Url -> User -> Pass -> Manager -> IO Page
downloadPage url user pass manager = do
  request <- basicAuthRequest url user pass
  response <- httpLbs request manager
  either fail return (eitherDecode $ responseBody response)

extractSshUrls :: Page -> [String]
extractSshUrls page = sshUrls
  where
    repos = values page
    clones = concatMap (clone . links) repos
    sshUrls = map href . filter (\clone -> name clone == "ssh") $ clones