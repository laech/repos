module Main where

import Control.Concurrent.Async
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Project.Bitbucket
import Project.Config
import Project.Git
import Project.Gitlab
import Project.Logging
import System.Environment
import System.Exit
import System.Log.Logger

main :: IO ()
main = do
  args <- getArgs
  case args of
    [configFile] -> process configFile >>= exitWith
    _ -> die "Usage: <this-program> <config-file>"

process :: FilePath -> IO ExitCode
process path = do
  setupLogger
  config <- loadConfig path
  infoM "Main" ("> " ++ directory config)
  manager <- newTlsManager
  allOk <$> mapConcurrently (>>= fetchRepos config) (getSshUrls manager config)
  where
    getSshUrls :: Manager -> Config -> [IO [String]]
    getSshUrls manager config = map (\f -> f manager config) getters
    getters = [getGitlabRepoSshUrls, getBitbucketRepoSshUrls]

fetchRepos :: Config -> [String] -> IO ExitCode
fetchRepos config sshUrls =
  allOk <$> mapConcurrently (fetchRepo $ directory config) sshUrls

allOk :: [ExitCode] -> ExitCode
allOk = foldl max ExitSuccess