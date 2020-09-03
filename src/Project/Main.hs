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
import Project.Gitlab
import Project.Logging
import System.Environment
import System.Exit
import System.Process

data Options = Options
  { getOptionDirectory :: FilePath,
    getOptionUsername :: String
  }
  deriving (Show, Eq)

options :: ParserM Options
options = do
  directory <- oneM $ Opt.strOption (Opt.long "directory")
  username <- oneM $ Opt.strOption (Opt.long "username")
  pure
    Options
      { getOptionDirectory = directory,
        getOptionUsername = username
      }

main :: IO ()
main = do
  setupLogger
  options <- Opt.execParser (Opt.info (fromM options <**> Opt.helper) Opt.fullDesc)
  result <- process options
  unless result exitFailure

process :: Options -> IO Bool
process options = do
  token <- getToken $getOptionUsername options
  manager <- newTlsManager
  results <- runAsync token manager
  and <$> mapM wait results
  where
    runAsync token manager = do
      let dir = getOptionDirectory options
      info $ "> " ++ dir
      sem <- newQSem 6
      forEachGitlabRepo
        manager
        token
        dir
        ( async
            . bracket_ (waitQSem sem) (signalQSem sem)
            . tryFetchRepo
        )

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

getToken :: String -> IO String
getToken username =
  readProcess
    "secret-tool"
    [ "lookup",
      "protocol",
      "https",
      "server",
      "gitlab.com",
      "user",
      username
    ]
    ""
