{-# LANGUAGE FlexibleContexts #-}

module Project.Git
  ( fetchRepo
  ) where

import Control.Monad.Base
import Data.List (intercalate)
import Project.Logging
import System.Directory
import System.Exit
import System.FilePath
import System.Process

getProjectName = dropExtension . takeFileName

execute :: MonadBase IO m => [CreateProcess] -> m (ExitCode, String, String)
execute [x] = liftBase $ readCreateProcessWithExitCode x ""
execute (x:xs) = do
  result@(code, _, _) <- execute [x]
  if code == ExitSuccess
    then execute xs
    else pure result

fetch :: FilePath -> CreateProcess
fetch dir = (shell "git fetch --all --quiet") {cwd = Just dir}

merge :: FilePath -> CreateProcess
merge dir = (shell "git merge FETCH_HEAD") {cwd = Just dir}

clone :: String -> FilePath -> CreateProcess
clone url parent = (shell $ "git clone --quiet " ++ url) {cwd = Just parent}

fetchRepo :: MonadBase IO m => FilePath -> String -> m ExitCode
fetchRepo parent url = do
  debug $ ". " ++ url
  let dst = parent </> getProjectName url
  exists <- liftBase $ doesDirectoryExist dst
  result@(code, _, _) <-
    if exists
      then execute [fetch dst, merge dst]
      else execute [clone url parent]
  info $ format url result
  pure code

format :: String -> (ExitCode, String, String) -> String
format url (code, out, err) =
  let (symbol, message) = status code
   in symbol ++ " " ++ url ++ message
  where
    status code =
      if code == ExitSuccess
        then ("✓", "")
        else ("✗", intercalate "\n" [out, err])
