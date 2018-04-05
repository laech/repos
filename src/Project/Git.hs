{-# LANGUAGE LambdaCase #-}

module Project.Git
  ( fetchRepo
  ) where

import Data.List (intercalate)
import System.Directory
import System.Exit
import System.FilePath
import System.Log.Logger
import System.Process

getProjectNameFromSshUrl :: String -> String
getProjectNameFromSshUrl = dropExtension . takeFileName

execute :: [CreateProcess] -> IO (ExitCode, String, String)
execute [x] = readCreateProcessWithExitCode x ""
execute (x:xs) = do
  result@(exitCode, _, _) <- execute [x]
  if exitCode == ExitSuccess
    then execute xs
    else return result

gitFetch :: FilePath -> CreateProcess
gitFetch directory = (shell "git fetch --all --quiet") {cwd = Just directory}

gitMerge :: FilePath -> CreateProcess
gitMerge directory = (shell "git merge FETCH_HEAD") {cwd = Just directory}

gitClone :: String -> FilePath -> CreateProcess
gitClone sshUrl parentDir =
  (shell $ "git clone --quiet " ++ sshUrl) {cwd = Just parentDir}

fetchRepo :: FilePath -> String -> IO ExitCode
fetchRepo parentDir sshUrl = do
  debugM "Git" (". " ++ sshUrl)
  let projectDir = parentDir </> getProjectNameFromSshUrl sshUrl
  result@(exitCode, _, _) <-
    doesDirectoryExist projectDir >>= \case
      True -> execute [gitFetch projectDir, gitMerge projectDir]
      _ -> readCreateProcessWithExitCode (gitClone sshUrl parentDir) ""
  infoM "Git" $ formatResult sshUrl result
  pure exitCode

getStatusSymbol exitCode =
  if exitCode == ExitSuccess
    then "✓"
    else "✗"

formatResult :: String -> (ExitCode, String, String) -> String
formatResult sshUrl (exitCode, out, err) =
  status ++ " " ++ sshUrl ++ message exitCode
  where
    status = getStatusSymbol exitCode
    message exitCode =
      if exitCode == ExitSuccess
        then ""
        else "\n" ++ intercalate "\n" (filter (not . null) [out, err])