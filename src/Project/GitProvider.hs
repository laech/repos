{-# LANGUAGE OverloadedStrings #-}

module Project.GitProvider
  ( gitlab,
    github,
    newGitLabProvider,
    newGitHubProvider,
    forEachRepo,
    Provider (..),
  )
where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Char8 (pack)
import Network.HTTP.Client
import Project.Git
import Project.Logging
import System.Directory
import System.FileLock
import System.FilePath

data Provider = Provider
  { getProviderRequest :: Request -> Request,
    getProviderName :: String,
    getProviderHost :: String,
    getProviderUser :: String,
    getProviderToken :: String,
    getProviderPageUrl :: Int -> String,
    getProviderRepoUrl :: Object -> Parser String
  }

github = "github"

gitlab = "gitlab"

approveProviderCredentials :: Provider -> IO ()
approveProviderCredentials provider =
  let [host, user, token] =
        [ getProviderHost,
          getProviderUser,
          getProviderToken
        ]
          <*> pure provider
   in approveCredential host user token

newGitLabProvider :: IO Provider
newGitLabProvider = do
  let host = "gitlab.com"
  (user, token) <- fillCredential host
  pure
    Provider
      { getProviderName = gitlab,
        getProviderHost = host,
        getProviderUser = user,
        getProviderToken = token,
        getProviderPageUrl = ("https://gitlab.com/api/v4/projects?owned=true&page=" ++) . show,
        getProviderRepoUrl = (.: "http_url_to_repo"),
        getProviderRequest = \req ->
          req
            { requestHeaders = [("Authorization", pack $ "Bearer " ++ token)]
            }
      }

newGitHubProvider :: IO Provider
newGitHubProvider = do
  let host = "github.com"
  (user, token) <- fillCredential host
  pure
    Provider
      { getProviderName = github,
        getProviderHost = host,
        getProviderUser = user,
        getProviderToken = token,
        getProviderPageUrl = ("https://api.github.com/user/repos?type=owner&page=" ++) . show,
        getProviderRepoUrl = (.: "clone_url"),
        getProviderRequest = \req ->
          applyBasicAuth (pack user) (pack token) $
            req
              { requestHeaders =
                  [ ("Accept", "application/vnd.github.v3+json"),
                    ("User-Agent", "repos")
                  ]
              }
      }

forEachRepo :: Provider -> Manager -> FilePath -> (Repo -> IO a) -> IO [a]
forEachRepo provider manager parent action = forEachPage initialPage
  where
    initialPage = 1
    forEachPage i = do
      repos <- getPage provider manager parent i
      when (i == initialPage) $approveProviderCredentials provider
      results <- mapM lockAction repos
      if null results
        then pure results
        else (results ++) <$> forEachPage (i + 1)
    lockAction repo = do
      let lock = getRepoParentDirectory repo </> (getRepoName repo ++ ".lock")
      withFileLock lock Exclusive (const . action $ repo)
        <* removeFile lock

getPage :: Provider -> Manager -> FilePath -> Int -> IO [Repo]
getPage provider manager parent page = do
  let url = getProviderPageUrl provider page
  debug (". " ++ url)
  request <- createRequest provider url
  response <- httpLbs request manager
  either fail (mapM parseRepo) (eitherDecode (responseBody response))
    <* info ("✓ " ++ url)
  where
    parseRepo repo =
      either
        fail
        (pure . toRepo)
        (parseEither (getProviderRepoUrl provider) repo)
    toRepo repoUrl =
      Repo
        { getRepoParentDirectory = parent,
          getRepoRemote =
            Remote
              { getRemoteName = getProviderName provider,
                getRemoteUrl = repoUrl
              }
        }

createRequest :: Provider -> String -> IO Request
createRequest provider url =
  parseUrlThrow url >>= \req ->
    pure $
      getProviderRequest
        provider
        req {responseTimeout = responseTimeoutMicro 60000000}
