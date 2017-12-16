module Main where

import Control.Concurrent.Async
import Control.Exception
import Data.Either
import Network.HTTP.Client.TLS
import Project.Bitbucket
import Project.Config
import Project.Git
import Project.Gitlab
import System.Environment
import System.Exit
import System.IO

main :: IO ()
main = do
  args <- getArgs
  case args of
    [configFile] -> process configFile >>= exitWith
    _ -> die "Usage: <this-program> <config-file>"

process :: FilePath -> IO ExitCode
process path = do
  config <- loadConfig path
  putStrLn $ "> " ++ directory config
  manager <- newTlsManager
  results <-
    mapConcurrently
      (processSshUrls (directory config))
      [ getGitlabRepoSshUrls manager (gitlabToken config)
      , getBitbucketRepoSshUrls
          manager
          (bitbucketUsername config)
          (bitbucketPassword config)
      ]
  return $
    if all (== ExitSuccess) results
      then ExitSuccess
      else ExitFailure 1

processSshUrls :: FilePath -> IO [String] -> IO ExitCode
processSshUrls directory sshUrls = do
  result <-
    try (sshUrls >>= fetchRepos directory) :: IO (Either SomeException ())
  case result of
    Left e -> print e >> return (ExitFailure 1)
    _ -> return ExitSuccess