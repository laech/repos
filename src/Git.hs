module Git
  ( fetchRepos
  ) where

import           Control.Concurrent.Async
import           Control.Monad
import           Data.List
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Process

getProjectNameFromSshUrl :: String -> String
getProjectNameFromSshUrl = dropExtension . takeFileName

-- Executes a process, if exit code is 1, execute the second.
execute1 :: CreateProcess -> CreateProcess -> IO (ExitCode, String, String)
execute1 p1 p2 = do
  result@(exitCode, _, _) <- readCreateProcessWithExitCode p1 ""
  case exitCode of
    ExitSuccess   -> return result
    ExitFailure _ -> readCreateProcessWithExitCode p2 ""

gitPull :: FilePath -> CreateProcess
gitPull directory = (shell "git pull -q") {cwd = Just directory}

gitMergeAbort :: FilePath -> CreateProcess
gitMergeAbort directory = (shell "git merge --abort") {cwd = Just directory}

gitClone :: String -> FilePath -> CreateProcess
gitClone sshUrl parentDir =
  (shell $ "git clone -q " ++ sshUrl) {cwd = Just parentDir}

getStatusSymbol :: ExitCode -> String
getStatusSymbol exitCode =
  case exitCode of
    ExitSuccess   -> "✓"
    ExitFailure _ -> "✗"

getOutputHandle :: ExitCode -> Handle
getOutputHandle exitCode =
  case exitCode of
    ExitSuccess   -> stdout
    ExitFailure _ -> stderr

fetchRepo :: FilePath -> String -> IO ExitCode
fetchRepo parentDir sshUrl = do
  let projectDir = parentDir </> getProjectNameFromSshUrl sshUrl
  projectDirExists <- doesDirectoryExist projectDir
  (exitCode, out, err) <-
    if projectDirExists
      then execute1 (gitPull projectDir) (gitMergeAbort projectDir)
      else readCreateProcessWithExitCode (gitClone sshUrl parentDir) ""
  hPutStrLn (getOutputHandle exitCode) $
    getStatusSymbol exitCode ++
    " " ++ sshUrl ++ intercalate "\n" (filter (not . null) [out, err])
  return exitCode

fetchRepos :: FilePath -> [String] -> IO ExitCode
fetchRepos parentDir sshUrls = do
  putStrLn $ "> " ++ parentDir
  results <- mapConcurrently (fetchRepo parentDir) sshUrls
  return $
    if all (== ExitSuccess) results
      then ExitSuccess
      else ExitFailure 1
