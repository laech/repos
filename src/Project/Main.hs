{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Applicative ((<**>))
import Control.Concurrent.Async (Async, async, wait)
import Control.Concurrent.QSem
  ( newQSem,
    signalQSem,
    waitQSem,
  )
import Control.Exception (bracket_)
import Data.Foldable (foldl')
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Options.Applicative
  ( Parser,
    execParser,
    fullDesc,
    helper,
    long,
    strOption,
  )
import qualified Options.Applicative as Opt
import Options.Applicative.Types (ParserM, fromM, oneM)
import Project.Git (fetchRepo)
import Project.Gitlab (forEachGitlabRepo)
import Project.Logging (info, setupLogger)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitSuccess), exitWith)
import System.Process (readProcess)

data Options = Options
  { directory :: FilePath,
    username :: String
  }
  deriving (Show)

options :: ParserM Options
options = do
  directory <- oneM $ strOption (long "directory")
  username <- oneM $ strOption (long "username")
  pure Options {directory, username}

main :: IO ()
main = do
  setupLogger
  options <- execParser (Opt.info (fromM options <**> helper) fullDesc)
  process options >>= exitWith

process :: Options -> IO ExitCode
process options = do
  token <- getToken (username options)
  manager <- newTlsManager
  results <- runAsync token manager
  allOk <$> mapM wait results
  where
    runAsync token manager = do
      info ("> " ++ directory options)
      sem <- newQSem 6
      forEachGitlabRepo
        manager
        token
        ( async
            . bracket_ (waitQSem sem) (signalQSem sem)
            . fetchRepo (directory options)
        )

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

allOk :: [ExitCode] -> ExitCode
allOk = foldl' max ExitSuccess
