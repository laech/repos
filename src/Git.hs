module Git (fetchRepos) where

import           Control.Exception (Exception, throwIO)
import           Control.Monad     (when)
import           Data.Typeable     (Typeable)
import           System.Directory  (doesDirectoryExist)
import           System.Exit       (ExitCode (ExitFailure, ExitSuccess))
import           System.FilePath   (dropExtension, takeFileName, (</>))
import           System.Process    (CreateProcess, createProcess, cwd, shell,
                                    waitForProcess)


newtype ProcessException = ProcessException String
  deriving (Typeable, Show)

instance Exception ProcessException


getProjectNameFromSshUrl :: String -> String
getProjectNameFromSshUrl = dropExtension . takeFileName


execute :: CreateProcess -> IO ()
execute p =
  createProcess p >>= \(_, _, _, handle) ->
  waitForProcess handle >>= \exitCode ->
  when (exitCode /= ExitSuccess)
    (throwIO (ProcessException (show exitCode)))


-- Executes a process, if exit code is 1, execute the second.
execute1 :: CreateProcess -> CreateProcess -> IO ()
execute1 p1 p2 = do
  (_, _, _, handle) <- createProcess p1
  exitCode <- waitForProcess handle
  case exitCode of
    ExitSuccess -> return ()
    ExitFailure r -> if r == 1
      then execute p2
      else (throwIO (ProcessException (show exitCode)))


gitPull :: FilePath -> CreateProcess
gitPull directory = (shell "git pull -v")
  { cwd = Just directory }


gitMergeAbort :: FilePath -> CreateProcess
gitMergeAbort directory = (shell "git merge --abort")
  { cwd = Just directory }


gitClone :: String -> FilePath -> CreateProcess
gitClone sshUrl parentDirectory = (shell $ "git clone -v " ++ sshUrl)
  { cwd = Just parentDirectory }


fetchRepo :: FilePath -> String -> IO ()
fetchRepo parentDirectory sshUrl = do
  let directory = parentDirectory </> getProjectNameFromSshUrl sshUrl
  exists <- doesDirectoryExist directory
  putStrLn ("\n> " ++ show directory)
  if exists
    then execute1 (gitPull directory) (gitMergeAbort directory)
    else execute  (gitClone sshUrl parentDirectory)


fetchRepos :: FilePath -> [String] -> IO ()
fetchRepos parentDirectory = mapM_ (fetchRepo parentDirectory)
