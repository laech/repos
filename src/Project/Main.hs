module Main where

import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.List
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
    [configFile] -> setupLogger >> process configFile >>= exitWith
    _ -> die "Usage: <this-program> <config-file>"

process :: FilePath -> IO ExitCode
process path = do
  config <- loadConfig path
  infoM "Main" ("> " ++ directory config)
  manager <- newTlsManager
  results <-
    mapConcurrently
      (processSshUrls (directory config))
      [ getGitlabRepoSshUrls' manager config
      , getBitbucketRepoSshUrls' manager config
      ]
  return $
    if all (== ExitSuccess) results
      then ExitSuccess
      else ExitFailure 1

getGitlabRepoSshUrls' manager config = do
  getGitlabRepoSshUrls manager (gitlabToken config)

getBitbucketRepoSshUrls' manager config = do
  getBitbucketRepoSshUrls
    manager
    (bitbucketUsername config)
    (bitbucketPassword config)

processSshUrls :: FilePath -> IO [String] -> IO ExitCode
processSshUrls directory sshUrls = do
  result <-
    try (sshUrls >>= fetchRepos directory) :: IO (Either SomeException ())
  case result of
    Left e -> errorM "Main" (show e) >> return (ExitFailure 1)
    _ -> return ExitSuccess

fetchRepos :: FilePath -> [String] -> IO ()
fetchRepos parentDir sshUrls = do
  results <- mapConcurrently fetch sshUrls
  when (any (/= ExitSuccess) results) $ throwIO $ ExitFailure 1
  where
    fetch :: String -> IO ExitCode
    fetch sshUrl = do
      debugM "Main" (". " ++ sshUrl)
      result@(exitCode, stdout, stderr) <- fetchRepo parentDir sshUrl
      infoM "Main" (formatResult sshUrl result)
      return exitCode

getStatusSymbol :: ExitCode -> String
getStatusSymbol exitCode =
  if exitCode == ExitSuccess
    then "✓"
    else "✗"

formatResult :: String -> (ExitCode, String, String) -> String
formatResult sshUrl (exitCode, out, err) =
  status ++ " " ++ sshUrl ++ message exitCode
  where
    status = getStatusSymbol exitCode
    message exitCode =
      if exitCode == ExitSuccess
        then ""
        else "\n" ++ intercalate "\n" (filter (not . null) [out, err])