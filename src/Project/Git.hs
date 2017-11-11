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

execute :: [CreateProcess] -> IO (ExitCode, String, String)
execute [x] = readCreateProcessWithExitCode x ""
execute (x:xs) = do
  result@(exitCode, _, _) <- execute [x]
  case exitCode of
    ExitSuccess   -> execute xs
    ExitFailure _ -> return result

gitFetch :: FilePath -> CreateProcess
gitFetch directory =
  (shell "git fetch --all --quiet")
    {cwd = Just directory}

gitMerge :: FilePath -> CreateProcess
gitMerge directory =
  (shell "git merge FETCH_HEAD")
    {cwd = Just directory}

gitClone :: String -> FilePath -> CreateProcess
gitClone sshUrl parentDir =
  (shell $ "git clone --quiet " ++ sshUrl)
    {cwd = Just parentDir}

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
doGitUpdate parentDir sshUrl = do
  let projectDir = parentDir </> getProjectNameFromSshUrl sshUrl
  exists <- doesDirectoryExist projectDir
  if exists
    then execute [gitFetch projectDir, gitMerge projectDir]
    else readCreateProcessWithExitCode (gitClone sshUrl parentDir) ""

printGitResult :: (ExitCode, String, String) -> String -> IO ()
printGitResult (exitCode, out, err) sshUrl = do
  hPutStr handle $ status ++ " " ++ sshUrl ++ "\n" ++ message exitCode
  hFlush handle
  where
    handle = getOutputHandle exitCode
    status = getStatusSymbol exitCode
    message exitCode =
      if exitCode == ExitSuccess
      then ""
      else intercalate "\n" (filter (not . null) [out, err])

fetchRepos :: FilePath -> [String] -> IO ()
fetchRepos parentDir sshUrls = do
  results <- mapConcurrently (fetchRepo parentDir) sshUrls
  when (any (/= ExitSuccess) results) $ throwIO $ ExitFailure 1
