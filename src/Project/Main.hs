{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified Pipes.Prelude as P

import Control.Concurrent.Async.Lifted
import Control.Monad.Base
import Control.Monad.Trans.Control
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Pipes
import Project.Bitbucket
import Project.Config
import Project.Git
import Project.Gitlab
import Project.Logging
import System.Environment
import System.Exit

main :: IO ()
main = do
  setupLogger
  args <- getArgs
  case args of
    [configFile] -> process configFile >>= exitWith
    _ -> die "Usage: <this-program> <config-file>"

process :: MonadBaseControl IO m => FilePath -> m ExitCode
process path = do
  config <- loadConfig path
  manager <- liftBase newTlsManager
  info $ "> " ++ directory config
  allOk <$> mapConcurrently (fetchRepos config) (getSshUrls manager config)
  where
    getSshUrls manager config = map (\f -> f manager config) getters
    getters = [getGitlabRepoSshUrls, getBitbucketRepoSshUrls]

fetchRepos ::
     MonadBaseControl IO m => Config -> Producer String m () -> m ExitCode
fetchRepos config sshUrls = do
  tasks <- P.toListM $ sshUrls >-> P.mapM asyncFetchRepo
  allOk <$> mapM wait tasks
  where
    asyncFetchRepo = async . fetchRepo (directory config)

allOk :: [ExitCode] -> ExitCode
allOk = foldl max ExitSuccess
