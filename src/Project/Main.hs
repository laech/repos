module Main where

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.QSem
import Control.Exception
import Control.Monad
import Network.HTTP.Client.TLS
import qualified Options.Applicative as Opt
import Options.Applicative.Types
import Project.Git
import Project.GitProvider
import Project.Logging
import System.Exit
import System.Process

data Options = Options
  { getOptionDirectory :: FilePath,
    getOptionGitLabUser :: String,
    getOptionGitHubUser :: String
  }
  deriving (Show, Eq)

options :: ParserM Options
options = do
  directory <- oneM $ Opt.strOption (Opt.long "directory")
  gitlabUser <- oneM $ Opt.strOption (Opt.long "gitlab-user")
  githubUser <- oneM $ Opt.strOption (Opt.long "github-user")
  pure
    Options
      { getOptionDirectory = directory,
        getOptionGitLabUser = gitlabUser,
        getOptionGitHubUser = githubUser
      }

main :: IO ()
main = do
  setupLogger
  options <- Opt.execParser (Opt.info (fromM options <**> Opt.helper) Opt.fullDesc)
  result <- process options
  unless result exitFailure

process :: Options -> IO Bool
process options = do
  gitlabToken <- getCredential "gitlab.com"
  githubToken <- getCredential "github.com"
  manager <- newTlsManager
  results <- runAsync gitlabToken githubToken manager
  and <$> mapM wait results
  where
    runAsync gitlabToken githubToken manager = do
      let dir = getOptionDirectory options
      info $ "> " ++ dir
      sem <- newQSem 6
      liftA2
        (++)
        ( forEachGitLabRepo
            gitlabToken
            manager
            dir
            (handle sem False)
        )
        ( forEachGitHubRepo
            (getOptionGitHubUser options)
            githubToken
            manager
            dir
            (handle sem True)
        )
    handle sem setAsOrigin =
      async
        . bracket_ (waitQSem sem) (signalQSem sem)
        . tryFetchRepo setAsOrigin

tryFetchRepo :: Bool -> Repo -> IO Bool
tryFetchRepo setAsOrigin repo = catch fetch handle
  where
    url = getRemoteUrl . getRepoRemote $ repo
    fetch = do
      debug $ ". " ++ url
      fetchRepo setAsOrigin repo
      info $ "✓ " ++ url
      pure True
    handle (SomeException e) = do
      error $ "✗ " ++ url ++ "\n" ++ show e
      pure False
