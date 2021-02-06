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

newtype Options = Options
  { getOptionDirectory :: FilePath
  }
  deriving (Show, Eq)

options :: ParserM Options
options = Options <$> oneM (Opt.strOption (Opt.long "directory"))

main :: IO ()
main = do
  setupLogger
  options <- Opt.execParser (Opt.info (fromM options <**> Opt.helper) Opt.fullDesc)
  result <- process options
  unless result exitFailure

process :: Options -> IO Bool
process options = do
  manager <- newTlsManager
  providers <- sequence [newGitLabProvider, newGitHubProvider]
  results <- runAsync providers manager
  and <$> mapM wait results
  where
    runAsync providers manager = do
      let dir = getOptionDirectory options
      info $ "> " ++ dir
      sem <- newQSem 6
      concat
        <$> mapM
          ( \provider ->
              forEachRepo
                provider
                manager
                dir
                (handle sem (getProviderName provider == github))
          )
          providers

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
