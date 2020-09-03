module Main where

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.QSem
import Control.Exception
import Control.Monad
import Data.Foldable
import Data.List
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Options.Applicative as Opt
import Options.Applicative.Types
import Project.Git
import Project.GitHub
import Project.GitLab
import Project.Logging
import System.Environment
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
  gitlabToken <- getGitLabToken $ getOptionGitLabUser options
  githubToken <- getGitHubToken $ getOptionGitHubUser options
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
            manager
            gitlabToken
            dir
            (handle sem)
        )
        ( forEachGitHubRepo
            manager
            (getOptionGitHubUser options)
            githubToken
            dir
            (handle sem)
        )
    handle sem =
      async
        . bracket_ (waitQSem sem) (signalQSem sem)
        . tryFetchRepo

tryFetchRepo :: Repo -> IO Bool
tryFetchRepo repo = catch fetch handle
  where
    url = getRemoteUrl . getRepoRemote $ repo
    fetch = do
      debug $ ". " ++ url
      fetchRepo repo
      info $ "✓ " ++ url
      pure True
    handle (SomeException e) = do
      error $ "✗ " ++ url ++ "\n" ++ show e
      pure False

getGitLabToken :: String -> IO String
getGitLabToken user =
  readProcess
    "secret-tool"
    [ "lookup",
      "protocol",
      "https",
      "server",
      "gitlab.com",
      "user",
      user
    ]
    ""

getGitHubToken :: String -> IO String
getGitHubToken user =
  readProcess
    "secret-tool"
    [ "lookup",
      "protocol",
      "https",
      "server",
      "github.com",
      "user",
      user
    ]
    ""
