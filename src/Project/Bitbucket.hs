{-# LANGUAGE OverloadedStrings #-}

module Project.Bitbucket
  ( getBitbucketRepoSshUrls
  ) where

import qualified Data.ByteString.Char8 as Char8
import qualified Data.HashMap.Strict as HM

import Control.Exception
import Control.Monad
import Control.Monad.Loops
import Data.Aeson
import Data.Aeson.Types
import Data.List
import Network.HTTP.Client
import Network.HTTP.Types.URI
import Network.URI

type Page = ([String], Maybe Url)

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
downloadSshUrls manager user pass url = concat <$> unfoldrM download (Just url)
  where
    download :: Maybe Url -> IO (Maybe Page)
    download Nothing = return Nothing
    download (Just url) = Just <$> downloadPage url user pass manager

basicAuthRequest :: Url -> User -> Pass -> IO Request
basicAuthRequest url user pass =
  applyBasicAuth (Char8.pack user) (Char8.pack pass) <$> parseUrlThrow url

downloadPage :: Url -> User -> Pass -> Manager -> IO Page
downloadPage url user pass manager = do
  request <- basicAuthRequest url user pass
  response <- httpLbs request manager
  either fail return (eitherDecode (responseBody response) >>= parseEither page)

page :: Value -> Parser Page
page =
  withObject "" $ \v -> do
    next <- v .:? "next"
    repos <- v .: "values" :: Parser [Object]
    links <- mapM (.: "links") repos
    clones <- filter isSsh . concat <$> mapM (.: "clone") links
    sshUrls <- mapM (.: "href") clones
    return (sshUrls, next)
  where
    isSsh clone = HM.lookup "name" clone == Just "ssh"