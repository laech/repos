{-# LANGUAGE FlexibleContexts #-}

module Project.Git
  ( fetchRepo,
  )
where

import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Project.Logging
import System.Directory
import System.Exit
import System.FilePath
import System.Process

type Url = String

getProjectName :: FilePath -> FilePath
getProjectName = dropExtension . takeFileName

execute :: NonEmpty CreateProcess -> IO (ExitCode, String, String)
execute (x :| []) = readCreateProcessWithExitCode x ""
execute (x :| y : ys) = execute (x :| []) >>= process
  where
    process (ExitSuccess, _, _) = execute (y :| ys)
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
    run False = execute (clone url parent :| [])
    run _ = execute (fetch dst :| [merge dst])
    completed result@(code, _, _) = code <$ info (format url result)

format :: Url -> (ExitCode, String, String) -> String
format url (code, out, err) =
  let (symbol, message) = status code
   in symbol ++ " " ++ url ++ message
  where
    status s =
      if s == ExitSuccess
        then ("✓", "")
        else ("✗", intercalate "\n" [out, err])
