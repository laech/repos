{-# LANGUAGE OverloadedStrings #-}

module Project.GitLab
  ( forEachGitLabRepo,
  )
where

import Data.Aeson
import qualified Data.ByteString.Char8 as C
import Network.HTTP.Client
import Project.Git
import Project.Logging (debug, info)

newtype GitLabRepo = GitLabRepo
  { getGitLabRepoUrl :: String
  }
  deriving (Show, Eq)

instance FromJSON GitLabRepo where
  parseJSON = withObject "GitLabRepo" $
    \v -> GitLabRepo <$> v .: "http_url_to_repo"

type Url = String

type Token = String

type PageNumber = Int

forEachGitLabRepo :: Manager -> Token -> FilePath -> (Repo -> IO a) -> IO [a]
forEachGitLabRepo manager token parent action = forEachPage 1
  where
    forEachPage i = do
      repos <- getPage manager token parent i
      results <- mapM action repos
      if null results
        then pure results
        else (results ++) <$> forEachPage (i + 1)

getPage :: Manager -> Token -> FilePath -> PageNumber -> IO [Repo]
getPage manager token parent page = do
  let url = "https://gitlab.com/api/v4/projects?owned=true&page=" ++ show page
  debug (". " ++ url)
  request <- createRequest url token
  response <- httpLbs request manager
  either fail (pure . fmap toRepo) (eitherDecode (responseBody response))
    <* info ("✓ " ++ url)
  where
    toRepo repo =
      Repo
        { getRepoParentDirectory = parent,
          getRepoRemote =
            Remote
              { getRemoteName = "gitlab",
                getRemoteUrl = getGitLabRepoUrl repo
              }
        }

createRequest :: Url -> Token -> IO Request
createRequest url token =
  parseUrlThrow url >>= \req ->
    pure
      req
        { requestHeaders = [("Private-Token", C.pack token)],
          responseTimeout = responseTimeoutMicro 60000000
        }
