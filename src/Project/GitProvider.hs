{-# LANGUAGE OverloadedStrings #-}

module Project.GitProvider
  ( forEachGitLabRepo,
    forEachGitHubRepo,
    Provider (..),
  )
where

import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Char8 (pack)
import Network.HTTP.Client
import Project.Git
import Project.Logging

data Provider = Provider
  { getProviderRequest :: Request -> Request,
    getProviderName :: String,
    getProviderPageUrl :: String -> String,
    getProviderRepoUrl :: Object -> Parser String
  }

forEachGitLabRepo :: String -> Manager -> FilePath -> (Repo -> IO a) -> IO [a]
forEachGitLabRepo token =
  forEachRepo
    Provider
      { getProviderName = "gitlab",
        getProviderPageUrl = ("https://gitlab.com/api/v4/projects?owned=true&page=" ++),
        getProviderRepoUrl = (.: "http_url_to_repo"),
        getProviderRequest = \req ->
          req
            { requestHeaders = [("Private-Token", pack token)]
            }
      }

forEachGitHubRepo ::
  String ->
  String ->
  Manager ->
  FilePath ->
  (Repo -> IO a) ->
  IO [a]
forEachGitHubRepo user token =
  forEachRepo
    Provider
      { getProviderName = "github",
        getProviderPageUrl = ("https://api.github.com/user/repos?type=owner&page=" ++),
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
forEachRepo provider manager parent action = forEachPage 1
  where
    forEachPage i = do
      repos <- getPage provider manager parent i
      results <- mapM action repos
      if null results
        then pure results
        else (results ++) <$> forEachPage (i + 1)

getPage :: Provider -> Manager -> FilePath -> Int -> IO [Repo]
getPage provider manager parent page = do
  let url = getProviderPageUrl provider (show page)
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
