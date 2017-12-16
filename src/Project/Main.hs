module Main where

import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.Either
import Data.List
import Network.HTTP.Client.TLS
import Project.Bitbucket
import Project.Config
import Project.Git
import Project.Gitlab
import System.Console.Concurrent
import System.Console.Regions
import System.Environment
import System.Exit
import System.IO

main :: IO ()
main =
  displayConsoleRegions $ do
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
      [ info "> Getting Gitlab repositories..." $
        getGitlabRepoSshUrls manager (gitlabToken config)
      , info "> Getting Bitbucket repositories..." $
        getBitbucketRepoSshUrls
          manager
          (bitbucketUsername config)
          (bitbucketPassword config)
      ]
  return $
    if all (== ExitSuccess) results
      then ExitSuccess
      else ExitFailure 1

info :: String -> IO a -> IO a
info msg io =
  withConsoleRegion Linear $ \region -> setConsoleRegion region msg >> io

processSshUrls :: FilePath -> IO [String] -> IO ExitCode
processSshUrls directory sshUrls = do
  result <-
    try (sshUrls >>= fetchRepos directory) :: IO (Either SomeException ())
  case result of
    Left e -> outputConcurrent (show e ++ "\n") >> return (ExitFailure 1)
    _ -> return ExitSuccess

fetchRepos :: FilePath -> [String] -> IO ()
fetchRepos parentDir sshUrls = do
  results <- mapConcurrently fetch sshUrls
  when (any (/= ExitSuccess) results) $ throwIO $ ExitFailure 1
  where
    fetch :: String -> IO ExitCode
    fetch sshUrl =
      withConsoleRegion Linear $ \region -> do
        setConsoleRegion region ("> " ++ sshUrl)
        result@(exitCode, stdout, stderr) <- fetchRepo parentDir sshUrl
        finishConsoleRegion region (formatResult sshUrl result)
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