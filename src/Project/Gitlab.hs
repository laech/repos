{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Project.Gitlab
  ( getGitlabRepoSshUrls
  ) where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import qualified Pipes.Aeson as PA

import Control.Monad
import Data.Aeson
import Data.ByteString (ByteString)
import GHC.Generics
import Network.HTTP.Client
import Project.Config
import Pipes
import Pipes.Parse
import System.Log.Logger

newtype Repo = Repo
  { sshUrl :: String
  } deriving (Show, Generic)

instance FromJSON Repo where
  parseJSON = withObject "Repo" $ \v -> Repo <$> v .: "ssh_url_to_repo"

getGitlabRepoSshUrls :: Manager -> Config -> Producer String IO ()
getGitlabRepoSshUrls manager config =
  for (repos manager $ gitlabToken config) $ yield . sshUrl

repos :: Manager -> String -> Producer Repo IO ()
repos manager token = do
  let url = "https://gitlab.com/api/v4/projects?owned=true"
  repos' <- lift $ readContent manager url token
  for (each repos') yield

readContent :: Manager -> String -> String -> IO [Repo]
readContent manager url token = do
  debugM "Gitlab" (". " ++ url)
  request <- withToken token <$> parseUrlThrow url
  withHTTP request manager $ \response -> do
    Just (Right items) <- evalStateT PA.decode (responseBody response)
    infoM "Gitlab" ("✓ " ++ url)
    pure items

withToken :: String -> Request -> Request
withToken token request =
  request {requestHeaders = [("Private-Token", C.pack token)]}

-- TODO remove after upgrading to a new stack LTS with pipes-http
withHTTP
    :: Request
    -> Manager
    -> (Response (Producer ByteString IO ()) -> IO a)
    -> IO a
withHTTP r m k = withResponse r m k'
  where
    k' resp = do
        let p = (from . brRead . responseBody) resp
        k (resp { responseBody = p})
    from :: IO ByteString -> Producer ByteString IO ()
    from io = go
      where
        go = do
          bs <- lift io
          unless (B.null bs) $ do
            yield bs
            go
