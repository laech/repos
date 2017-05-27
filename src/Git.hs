module Git (fetchRepos) where

import           Control.Exception (Exception, throwIO)
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
execute p = do
  (_, _, _, handle) <- createProcess p
  exitCode <- waitForProcess handle
  case exitCode of
    ExitSuccess -> return ()
    _           -> throwIO (ProcessException (show exitCode))


gitFetch :: FilePath -> CreateProcess
gitFetch directory = (shell "git fetch -v")
  { cwd = Just directory }


gitClone :: String -> FilePath -> CreateProcess
gitClone sshUrl parentDirectory = (shell $ "git clone -v " ++ sshUrl)
  { cwd = Just parentDirectory }


fetchRepo :: FilePath -> String -> IO ()
fetchRepo parentDirectory sshUrl = do
  let directory = parentDirectory </> getProjectNameFromSshUrl sshUrl
  exists <- doesDirectoryExist directory
  execute $ if exists
    then gitFetch directory
    else gitClone sshUrl parentDirectory


fetchRepos :: FilePath -> [String] -> IO ()
fetchRepos parentDirectory = mapM_ (fetchRepo parentDirectory)
