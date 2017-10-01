module Main where

import qualified Bitbucket
import qualified Gitlab

import           Config
import           Git
import           System.Environment
import           System.Exit

main :: IO ()
main = do
  args <- getArgs
  case args of
    [configFile] -> process configFile >>= exitWith
    _            -> die "Usage: <this-program> <config-file>"

process :: FilePath -> IO ExitCode
process path = do
  config <- Config.load path
  gitlabUrls <- getRepoSshUrlsFromGitlab config
  bitbucketUrls <- getRepoSshUrlsFromBitbucket config
  fetchRepos (directory config) (gitlabUrls ++ bitbucketUrls)

getRepoSshUrlsFromGitlab :: Config -> IO [String]
getRepoSshUrlsFromGitlab config = Gitlab.getRepoSshUrls (gitlabToken config)

getRepoSshUrlsFromBitbucket :: Config -> IO [String]
getRepoSshUrlsFromBitbucket config =
  Bitbucket.getRepoSshUrls (bitbucketUsername config) (bitbucketPassword config)
