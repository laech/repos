{-# LANGUAGE DeriveGeneric #-}

module Bitbucket
  ( getBitbucketRepoSshUrls
  ) where

import qualified Data.ByteString.Char8      as Char8
import qualified Data.ByteString.Lazy.Char8 as LazyChar8
import qualified Network.HTTP.Client        as Http

import           Data.Aeson                 (FromJSON, eitherDecode)
import           Data.List
import           GHC.Generics
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.URI

data Page = Page
  { values :: [Repository]
  , next   :: Maybe String
  } deriving (Generic, Show)

newtype Repository = Repository
  { links :: Links
  } deriving (Generic, Show)

newtype Links = Links
  { clone :: [Clone]
  } deriving (Generic, Show)

data Clone = Clone
  { name :: String
  , href :: Url
  } deriving (Generic, Show)

instance FromJSON Page

instance FromJSON Repository

instance FromJSON Links

instance FromJSON Clone

type Url = String

type User = String

type Pass = String

type Result a = IO (Either String a)

getBitbucketRepoSshUrls :: String -> String -> Result [String]
getBitbucketRepoSshUrls username password =
  let url = "https://bitbucket.org/api/2.0/repositories/" ++ username
      manager = Http.newManager tlsManagerSettings
  in downloadSshUrls url username password =<< manager

downloadSshUrls :: Url -> User -> Pass -> Http.Manager -> Result [String]
downloadSshUrls url user pass manager = do
  pages <- downloadPages (Just url) user pass manager
  return $ concatMap extractSshUrls <$> pages

downloadPages :: Maybe Url -> User -> Pass -> Http.Manager -> Result [Page]
downloadPages Nothing _ _ _ = pure $ Right []
downloadPages (Just url) user pass manager = do
  page <- downloadPage url user pass manager
  pages <- downloadPages (nextPageUrl page) user pass manager
  return $ (:) <$> page <*> pages

nextPageUrl :: Either a Page -> Maybe Url
nextPageUrl (Right page) = next page
nextPageUrl _            = Nothing

basicAuthRequest :: Url -> User -> Pass -> IO Http.Request
basicAuthRequest url user pass =
  applyBasicAuth (Char8.pack user) (Char8.pack pass) <$> parseUrlThrow url

downloadPage :: Url -> User -> Pass -> Http.Manager -> Result Page
downloadPage url user pass manager = do
  request <- basicAuthRequest url user pass
  response <- httpLbs request manager
  pure . eitherDecode . responseBody $ response

extractSshUrls :: Page -> [String]
extractSshUrls page = sshUrls
  where
    repos = values page
    clones = concatMap (clone . links) repos
    sshUrls = map href . filter (\clone -> name clone == "ssh") $ clones
