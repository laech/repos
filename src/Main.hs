module Main where

import qualified Bitbucket
import qualified Gitlab

import           Config
import           Git
import           System.Environment (getArgs)
import           System.Exit        (die)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [configFile] -> process configFile
    _            -> die "Usage: <this-program> <config-file>"


process :: FilePath -> IO ()
process configFile = do
  config <- Config.load configFile
  (++) <$> getRepoSshUrlsFromGitlab config
       <*> getRepoSshUrlsFromBitbucket config
       >>= fetchRepos (directory config)


getRepoSshUrlsFromGitlab :: Config -> IO [String]
getRepoSshUrlsFromGitlab config =
  Gitlab.getRepoSshUrls (gitlabToken config)


getRepoSshUrlsFromBitbucket :: Config -> IO [String]
getRepoSshUrlsFromBitbucket config =
  Bitbucket.getRepoSshUrls
    (bitbucketUsername config)
    (bitbucketPassword config)
