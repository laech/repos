{-# LANGUAGE OverloadedStrings #-}

module Project.GitHub
  ( forEachGitHubRepo,
  )
where

import Data.Aeson
import qualified Data.ByteString.Char8 as C
import Network.HTTP.Client
import Project.Git
import Project.Logging (debug, info)

newtype GitHubRepo = GitHubRepo
  { getGitHubRepoUrl :: String
  }
  deriving (Show, Eq)

instance FromJSON GitHubRepo where
  parseJSON = withObject "GitHubRepo" $
    \v -> GitHubRepo <$> v .: "clone_url"

type Url = String

type User = String

type Token = String

type PageNumber = Int

forEachGitHubRepo :: Manager -> User -> Token -> FilePath -> (Repo -> IO a) -> IO [a]
forEachGitHubRepo manager user token parent action = forEachPage 1
  where
    forEachPage i = do
      repos <- getPage manager user token parent i
      results <- mapM action repos
      if null results
        then pure results
        else (results ++) <$> forEachPage (i + 1)

getPage :: Manager -> User -> Token -> FilePath -> PageNumber -> IO [Repo]
getPage manager user token parent page = do
  let url = "https://api.github.com/user/repos?page=" ++ show page
  debug (". " ++ url)
  request <- createRequest url user token
  response <- httpLbs request manager
  either fail (pure . fmap toRepo) (eitherDecode (responseBody response))
    <* info ("✓ " ++ url)
  where
    toRepo repo =
      Repo
        { getRepoParentDirectory = parent,
          getRepoRemote =
            Remote
              { getRemoteName = "github",
                getRemoteUrl = getGitHubRepoUrl repo
              }
        }

createRequest :: Url -> User -> Token -> IO Request
createRequest url user token =
  parseUrlThrow url >>= \req ->
    pure $
      applyBasicAuth (C.pack user) (C.pack token) $
        req
          { requestHeaders =
              [ ("Accept", "application/vnd.github.v3+json"),
                ("User-Agent", "repos")
              ],
            responseTimeout = responseTimeoutMicro 60000000
          }
