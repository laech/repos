module Main where

import           Bitbucket
import           Config
import           Control.Concurrent.Async
import           Data.Either
import           Git
import           Gitlab
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
      ((=<<) $ processSshUrls (directory config))
      [ getGitlabRepoSshUrls (gitlabToken config)
      , getBitbucketRepoSshUrls
          (bitbucketUsername config)
          (bitbucketPassword config)
      ]
  return $
    if all (== ExitSuccess) results
      then ExitSuccess
      else ExitFailure 1

processSshUrls :: FilePath -> Either String [String] -> IO ExitCode
processSshUrls directory sshUrls =
  case sshUrls of
    Right urls -> fetchRepos directory urls
    Left err   -> hPutStrLn stderr err >> return (ExitFailure 1)
