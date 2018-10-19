{-# LANGUAGE FlexibleContexts #-}

module Project.Git
  ( fetchRepo
  ) where

import Data.Functor
import Data.List (intercalate)
import Project.Logging
import System.Directory
import System.Exit
import System.FilePath
import System.Process

type Url = String

getProjectName = dropExtension . takeFileName

execute :: [CreateProcess] -> IO (ExitCode, String, String)
execute [x] = readCreateProcessWithExitCode x ""
execute (x:xs) = execute [x] >>= process
  where
    process (ExitSuccess, _, _) = execute xs
    process failure = pure failure

fetch :: FilePath -> CreateProcess
fetch dir = (shell "git fetch --all --quiet") {cwd = Just dir}

merge :: FilePath -> CreateProcess
merge dir = (shell "git merge FETCH_HEAD") {cwd = Just dir}

clone :: Url -> FilePath -> CreateProcess
clone url parent = (shell $ "git clone --quiet " ++ url) {cwd = Just parent}

fetchRepo :: FilePath -> Url -> IO ExitCode
fetchRepo parent url =
  debug (". " ++ url) *> doesDirectoryExist dst >>= run >>= completed
  where
    dst = parent </> getProjectName url
    run False = execute [clone url parent]
    run _ = execute [fetch dst, merge dst]
    completed result@(code, _, _) = code <$ info (format url result)

format :: Url -> (ExitCode, String, String) -> String
format url (code, out, err) =
  let (symbol, message) = status code
   in symbol ++ " " ++ url ++ message
  where
    status code =
      if code == ExitSuccess
        then ("✓", "")
        else ("✗", intercalate "\n" [out, err])
