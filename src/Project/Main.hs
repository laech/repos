module Main where

import Control.Applicative ((<**>))
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.QSem
  ( newQSem,
    signalQSem,
    waitQSem,
  )
import Control.Exception (bracket_)
import Network.HTTP.Client.TLS (newTlsManager)
import qualified Options.Applicative as Opt
import Options.Applicative
  ( Parser,
    execParser,
    fullDesc,
    helper,
    long,
    strOption,
  )
import Options.Applicative.Types (ParserM, fromM, oneM)
import Pipes ((>->), Producer)
import qualified Pipes.Prelude as P
import Project.Git (fetchRepo)
import Project.Gitlab (getGitlabRepoUrls)
import Project.Logging (info, setupLogger)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitSuccess), exitWith)
import System.Process (readProcess)

data Options
  = Options
      { directory :: FilePath,
        username :: String
      }
  deriving (Show)

options :: ParserM Options
options = do
  directory <- oneM $ strOption (long "directory")
  username <- oneM $ strOption (long "username")
  pure
    Options
      { directory = directory,
        username = username
      }

main :: IO ()
main = do
  _ <- setupLogger
  options <- execParser (Opt.info (fromM options <**> helper) fullDesc)
  process options >>= exitWith

process :: Options -> IO ExitCode
process options = do
  token <- getToken (username options)
  man <- newTlsManager
  run token man
  where
    run token man =
      info ("> " ++ directory options)
        *> fetchRepos (directory options) (getGitlabRepoUrls man token)

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

fetchRepos :: FilePath -> Producer String IO () -> IO ExitCode
fetchRepos directory urls = do
  sem <- newQSem 6
  asyncExitCodes <- P.toListM $ urls >-> P.mapM (asyncFetchRepo sem)
  fmap allOk . mapM wait $ asyncExitCodes
  where
    asyncFetchRepo sem url =
      async $
        bracket_
          (waitQSem sem)
          (signalQSem sem)
          (fetchRepo directory url)

allOk :: [ExitCode] -> ExitCode
allOk = foldl max ExitSuccess
