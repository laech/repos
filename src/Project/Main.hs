module Main where

import           Control.Concurrent.Async
import           Control.Exception
import           Data.Either
import           Project.Bitbucket
import           Project.Config
import           Project.Git
import           Project.Gitlab
import           System.Environment
import           System.Exit
import           System.IO

main :: IO ()
main = do
  args <- getArgs
  case args of
    [configFile] -> process configFile >>= exitWith
    _            -> die "Usage: <this-program> <config-file>"

process :: FilePath -> IO ExitCode
process path = do
  config <- loadConfig path
  putStrLn $ "> " ++ directory config
  results <-
    mapConcurrently
      (processSshUrls (directory config))
      [ getGitlabRepoSshUrls (gitlabToken config)
      , getBitbucketRepoSshUrls
          (bitbucketUsername config)
          (bitbucketPassword config)
      ]
  return $
    if all (== ExitSuccess) results
      then ExitSuccess
      else ExitFailure 1

processSshUrls :: FilePath -> IO [String] -> IO ExitCode
processSshUrls directory getSshUrls = do
  result <-
    try (getSshUrls >>= fetchRepos directory) :: IO (Either ExitCode ())
  case result of
    Left e -> return e
    _      -> return ExitSuccess
