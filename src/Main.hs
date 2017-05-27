module Main where

import qualified Config

import           Bitbucket          (getProjectSshUrls)
import           Git                (fetchRepos)
import           System.Environment (getArgs)
import           System.Exit        (die)


process :: FilePath -> IO ()
process configFile = do

  config <- Config.load configFile
  sshUrls <- getProjectSshUrls
    (Config.username config)
    (Config.password config)

  if null sshUrls
    then putStrLn "No project found."
    else fetchRepos (Config.directory config) sshUrls


main :: IO ()
main = do
  args <- getArgs
  case args of
    [configFile] -> process configFile
    _            -> die "Usage: <this-program> <config-file>"
