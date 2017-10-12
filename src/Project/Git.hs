module Project.Git
  ( fetchRepos
  ) where

import           Control.Concurrent.Async
import           Control.Exception
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
  result@(exitCode, out, err) <- doGitUpdate parentDir sshUrl
  printGitResult result sshUrl
  return exitCode

doGitUpdate :: FilePath -> String -> IO (ExitCode, String, String)
doGitUpdate parentDir sshUrl =
  let projectDir = parentDir </> getProjectNameFromSshUrl sshUrl
  in doesDirectoryExist projectDir >>= \exists ->
       if exists
         then execute1 (gitPull projectDir) (gitMergeAbort projectDir)
         else readCreateProcessWithExitCode (gitClone sshUrl parentDir) ""

printGitResult :: (ExitCode, String, String) -> String -> IO ()
printGitResult (exitCode, out, err) sshUrl =
  hPutStr handle $ status ++ " " ++ sshUrl ++ "\n" ++ message
  where
    handle = getOutputHandle exitCode
    status = getStatusSymbol exitCode
    message = intercalate "\n" (filter (not . null) [out, err])

fetchRepos :: FilePath -> [String] -> IO ()
fetchRepos parentDir sshUrls = do
  results <- mapConcurrently (fetchRepo parentDir) sshUrls
  when (any (/= ExitSuccess) results) $ throwIO $ ExitFailure 1
