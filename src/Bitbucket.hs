{-# LANGUAGE DeriveGeneric #-}

module Bitbucket (getRepoSshUrls) where

import qualified Data.ByteString.Char8      as Char8
import qualified Data.ByteString.Lazy.Char8 as LazyChar8
import qualified Network.HTTP.Client        as Http

import           Control.Exception          (Exception, throw)
import           Data.Aeson                 (FromJSON, eitherDecode)
import           Data.List                  (isPrefixOf, sort)
import           Data.Typeable              (Typeable)
import           GHC.Generics               (Generic)
import           Network.HTTP.Client        (applyBasicAuth, httpLbs,
                                             parseUrlThrow, responseBody)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)


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


getRepoSshUrls :: String -> String -> IO [String]
getRepoSshUrls username password =
  let url = "https://bitbucket.org/api/2.0/repositories/" ++ username
      manager = Http.newManager tlsManagerSettings
  in sort <$> (downloadSshUrls url username password =<< manager)


downloadSshUrls :: Url -> User -> Pass -> Http.Manager -> IO [String]
downloadSshUrls url user pass manager =
  concatMap extractSshUrls <$> downloadPages (Just url) user pass manager


downloadPages :: Maybe Url -> User -> Pass -> Http.Manager -> IO [Page]
downloadPages Nothing _ _ _ = pure []
downloadPages (Just url) user pass manager =
  downloadPage url user pass manager >>= \page ->
  (page:) <$> downloadPages (next page) user pass manager


basicAuthRequest :: Url -> User -> Pass -> IO Http.Request
basicAuthRequest url user pass =
  applyBasicAuth (Char8.pack user) (Char8.pack pass) <$>
  parseUrlThrow url


downloadPage :: Url -> User -> Pass -> Http.Manager -> IO Page
downloadPage url user pass manager =
  basicAuthRequest url user pass >>= \request ->
  httpLbs request manager >>= \response ->
  pure . parsePage . responseBody $ response


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
    sshUrls = filter (isPrefixOf "ssh:") hrefs
