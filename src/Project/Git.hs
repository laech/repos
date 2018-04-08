module Project.Git
  ( fetchRepo
  ) where

import Data.List (intercalate)
import System.Directory
import System.Exit
import System.FilePath
import System.Log.Logger
import System.Process

getProjectName = dropExtension . takeFileName

execute :: [CreateProcess] -> IO (ExitCode, String, String)
execute [x] = readCreateProcessWithExitCode x ""
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

fetchRepo :: FilePath -> String -> IO ExitCode
fetchRepo parent url = do
  debugM "Git" (". " ++ url)
  let dst = parent </> getProjectName url
  exists <- doesDirectoryExist dst
  result@(code, _, _) <-
    if exists
      then execute [fetch dst, merge dst]
      else readCreateProcessWithExitCode (clone url parent) ""
  infoM "Git" $ format url result
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
