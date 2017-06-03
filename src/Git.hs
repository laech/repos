module Git (fetchRepos) where

import           Control.Exception (Exception, throwIO)
import           Control.Monad     (when)
import           Data.Typeable     (Typeable)
import           System.Directory  (doesDirectoryExist)
import           System.Exit       (ExitCode (ExitSuccess))
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


gitFetch :: FilePath -> CreateProcess
gitFetch directory = (shell "git fetch -v")
  { cwd = Just directory }


gitClone :: String -> FilePath -> CreateProcess
gitClone sshUrl parentDirectory = (shell $ "git clone -v " ++ sshUrl)
  { cwd = Just parentDirectory }


fetchRepo :: FilePath -> String -> IO ()
fetchRepo parentDirectory sshUrl =
  let directory = parentDirectory </> getProjectNameFromSshUrl sshUrl
      exists = doesDirectoryExist directory
  in exists >>= \exists -> execute $ if exists
    then gitFetch directory
    else gitClone sshUrl parentDirectory


fetchRepos :: FilePath -> [String] -> IO ()
fetchRepos parentDirectory = mapM_ (fetchRepo parentDirectory)
