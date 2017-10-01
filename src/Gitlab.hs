{-# LANGUAGE DeriveGeneric #-}

module Gitlab (getRepoSshUrls) where

import qualified Data.ByteString.Char8      as Char8
import qualified Data.ByteString.Lazy.Char8 as LazyChar8
import qualified Network.HTTP.Client        as Http

import           Control.Exception          (Exception, throw)
import           Data.Aeson                 (FromJSON, eitherDecode)
import           Data.Typeable              (Typeable)
import           GHC.Generics               (Generic)
import           Network.HTTP.Client        (httpLbs, parseUrlThrow,
                                             responseBody)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)


newtype Repo = Repo { ssh_url_to_repo :: String }
  deriving (Show, Generic)

instance FromJSON Repo


newtype JsonException = JsonException String
  deriving (Show, Typeable)

instance Exception JsonException


type Token = String
type Url = String


getRepoSshUrls :: Token -> IO [String]
getRepoSshUrls token =
  let url = "https://gitlab.com/api/v4/projects?owned=true&private_token=" ++ token
      manager = Http.newManager tlsManagerSettings
  in map ssh_url_to_repo <$> (getRepos url =<< manager)


getRepos :: Url -> Http.Manager -> IO [Repo]
getRepos url manager =
  parseUrlThrow url >>= \request ->
  httpLbs request manager >>= \response ->
  pure . parseRepos . responseBody $ response


parseRepos :: LazyChar8.ByteString -> [Repo]
parseRepos json =
  case eitherDecode json :: Either String [Repo] of
    Left err    -> throw . JsonException  $ err ++ "\n" ++ LazyChar8.unpack json
    Right repos -> repos
