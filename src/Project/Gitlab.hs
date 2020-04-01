{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Project.Gitlab
  ( forEachGitlabRepo,
  )
where

import Data.Aeson
  ( (.:),
    FromJSON,
    eitherDecode,
    parseJSON,
    withObject,
  )
import qualified Data.ByteString.Char8 as C
import GHC.Generics (Generic)
import Network.HTTP.Client
  ( Manager,
    Request (requestHeaders, responseTimeout),
    httpLbs,
    parseUrlThrow,
    responseBody,
    responseTimeoutMicro,
  )
import Project.Logging (debug, info)

newtype Repo = Repo {repoUrl :: String}
  deriving (Show, Eq, Generic)

instance FromJSON Repo where
  parseJSON = withObject "Repo" $ \v -> Repo <$> v .: "http_url_to_repo"

type Url = String

type Token = String

type PageNumber = Int

forEachGitlabRepo :: Manager -> Token -> (Url -> IO a) -> IO [a]
forEachGitlabRepo manager token action = forEachPage 1
  where
    forEachPage i = do
      repos <- getPage manager token i
      results <- traverse (action . repoUrl) repos
      if null results
        then pure results
        else (results ++) <$> forEachPage (i + 1)

getPage :: Manager -> Token -> PageNumber -> IO [Repo]
getPage manager token page = do
  let url = "https://gitlab.com/api/v4/projects?owned=true&page=" ++ show page
  debug (". " ++ url)
  request <- createRequest url token
  response <- httpLbs request manager
  either fail pure (eitherDecode (responseBody response))
    <* info ("✓ " ++ url)

createRequest :: Url -> Token -> IO Request
createRequest url token = parseUrlThrow url >>= \req ->
  pure
    req
      { requestHeaders = [("Private-Token", C.pack token)],
        responseTimeout = responseTimeoutMicro 60000000
      }
