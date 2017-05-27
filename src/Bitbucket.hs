{-# LANGUAGE DeriveGeneric #-}

module Bitbucket (getProjectSshUrls) where

import qualified Data.ByteString.Char8      as Char8
import qualified Data.ByteString.Lazy.Char8 as LazyChar8
import qualified Data.List                  as List
import qualified Network.HTTP.Client        as Http

import           Control.Exception          (Exception, throw)
import           Data.Aeson                 (FromJSON, eitherDecode)
import           Data.Typeable              (Typeable)
import           GHC.Generics               (Generic)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Network.HTTP.Types.Status  (statusCode)


data Page = Page { values :: [Repository], next :: Maybe String }
  deriving (Generic, Show)


newtype Repository = Repository { links :: Links }
  deriving (Generic, Show)


newtype Links = Links { clone :: [Clone] }
  deriving (Generic, Show)


data Clone = Clone { name :: String, href :: Url }
  deriving (Generic, Show)


instance FromJSON Page
instance FromJSON Repository
instance FromJSON Links
instance FromJSON Clone


newtype JsonException = JsonException String
  deriving (Typeable, Show)

instance Exception JsonException


type Url = String
type User = String
type Pass = String


getProjectSshUrls :: String -> String -> IO [String]
getProjectSshUrls username password = do
  manager <- Http.newManager tlsManagerSettings
  List.sort <$> downloadSshUrls
    ("https://bitbucket.org/api/2.0/repositories/" ++ username)
    username
    password
    manager


downloadSshUrls :: Url -> User -> Pass -> Http.Manager -> IO [String]
downloadSshUrls url user pass manager = do
  pages <- downloadPages (Just url) user pass manager
  return $ concatMap extractSshUrls pages


downloadPages :: Maybe Url -> User -> Pass -> Http.Manager -> IO [Page]
downloadPages Nothing _ _ _ = return []
downloadPages (Just url) user pass manager = do
  page <- downloadPage url user pass manager
  (page:) <$> downloadPages (next page) user pass manager


basicAuthRequest :: Url -> User -> Pass -> IO Http.Request
basicAuthRequest url user pass = do
  request <- Http.parseUrlThrow url
  return $ Http.applyBasicAuth
    (Char8.pack user)
    (Char8.pack pass)
    request


downloadPage :: Url -> User -> Pass -> Http.Manager -> IO Page
downloadPage url user pass manager = do
  request <- basicAuthRequest url user pass
  response <- Http.httpLbs request manager
  return . parsePage . Http.responseBody $ response


parsePage :: LazyChar8.ByteString -> Page
parsePage json =
  case eitherDecode json :: Either String Page of
    Left err   -> throw . JsonException $ err ++ "\n" ++ LazyChar8.unpack json
    Right page -> page


extractSshUrls :: Page -> [String]
extractSshUrls page = sshUrls
  where
    repos = values page
    clones = concatMap (clone . links) repos
    hrefs = map href clones
    sshUrls = filter (List.isPrefixOf "ssh:") hrefs
