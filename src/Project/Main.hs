{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Pipes.Prelude as P

import Control.Applicative
import Control.Concurrent.Async
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
main =
  setupLogger *> getArgs >>= \case
    [configFile] -> process configFile >>= exitWith
    _ -> die "Usage: <this-program> <config-file>"

process :: FilePath -> IO ExitCode
process path = (,) <$> loadConfig path <*> newTlsManager >>= uncurry run
  where
    getSshUrls man conf =
      map (\f -> f man conf) [getGitlabRepoSshUrls, getBitbucketRepoSshUrls]
    run conf man =
      info ("> " ++ directory conf) *>
      (allOk <$> mapConcurrently (fetchRepos conf) (getSshUrls man conf))

fetchRepos :: Config -> Producer String IO () -> IO ExitCode
fetchRepos conf sshUrls =
  P.toListM (sshUrls >-> P.mapM asyncFetchRepo) >>= fmap allOk . mapM wait
  where
    asyncFetchRepo = async . fetchRepo (directory conf)

allOk :: [ExitCode] -> ExitCode
allOk = foldl max ExitSuccess
