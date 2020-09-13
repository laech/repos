module Project.Git
  ( fetchRepo,
    getCredential,
    Repo (..),
    Remote (..),
  )
where

import Control.Applicative hiding (many)
import Control.Monad
import Data.Char
import Data.List
import System.Directory
import System.Exit
import System.FilePath
import System.Process
import Text.ParserCombinators.ReadP

data Remote = Remote
  { getRemoteName :: String,
    getRemoteUrl :: String
  }
  deriving (Show, Eq)

data Repo = Repo
  { getRepoParentDirectory :: FilePath,
    getRepoRemote :: Remote
  }
  deriving (Show, Eq)

getRepoName :: Repo -> String
getRepoName =
  dropExtension
    . takeFileName
    . getRemoteUrl
    . getRepoRemote

getRepoDirectory :: Repo -> FilePath
getRepoDirectory repo =
  getRepoParentDirectory repo </> getRepoName repo

execute :: CreateProcess -> IO String
execute cmd = do
  (code, out, err) <- readCreateProcessWithExitCode cmd ""
  if code == ExitSuccess
    then pure out
    else fail $ intercalate "\n" [show cmd, out, err]

execute_ :: CreateProcess -> IO ()
execute_ cmd = do
  execute cmd
  pure ()

remote :: ReadP Remote
remote = do
  let textThenSpaces = many1 (satisfy $ not . isSpace) <* skipSpaces
  name <- textThenSpaces
  url <- textThenSpaces
  string "(fetch)" <|> string "(push)"
  skipSpaces
  pure $
    Remote
      { getRemoteName = name,
        getRemoteUrl = url
      }

readRemotes :: Repo -> IO [Remote]
readRemotes repo = do
  parse
    =<< execute
      (proc "git" ["remote", "-v"])
        { cwd = Just . getRepoDirectory $ repo
        }
  where
    parse output = do
      case readP_to_S (many remote <* eof) output of
        [(remotes, "")] -> pure . nub $remotes
        x -> fail $ "Unable to parse remotes.\n" ++ show x

addRemote :: Repo -> IO ()
addRemote repo = do
  let remote = getRepoRemote repo
  execute_
    ( proc
        "git"
        [ "remote",
          "add",
          getRemoteName remote,
          getRemoteUrl remote
        ]
    )
      { cwd = Just $ getRepoDirectory repo
      }

setRemote :: Repo -> IO ()
setRemote repo = do
  let remote = getRepoRemote repo
  execute_
    ( proc
        "git"
        [ "remote",
          "set-url",
          getRemoteName remote,
          getRemoteUrl remote
        ]
    )
      { cwd = Just $ getRepoDirectory repo
      }

fetch :: Repo -> IO ()
fetch repo = do
  let remote = getRepoRemote repo
  remotes <- readRemotes repo
  unless (remote `elem` remotes) $
    if getRemoteName remote `elem` fmap getRemoteName remotes
      then setRemote repo
      else addRemote repo
  execute_
    (proc "git" ["fetch", "--quiet", getRemoteName . getRepoRemote $ repo])
      { cwd = Just $ getRepoDirectory repo
      }

clone :: Repo -> IO ()
clone repo = do
  let remote = getRepoRemote repo
  execute_
    ( proc
        "git"
        [ "clone",
          "--quiet",
          "--origin",
          getRemoteName remote,
          getRemoteUrl remote
        ]
    )
      { cwd = Just . takeDirectory . getRepoDirectory $ repo
      }

fetchRepo :: Bool -> Repo -> IO ()
fetchRepo setAsOrogin repo = do
  let url = getRemoteUrl . getRepoRemote $ repo
  let dst = getRepoDirectory repo
  exists <- doesDirectoryExist dst
  (if exists then fetch else clone) repo
  when setAsOrogin $
    fetch
      repo
        { getRepoRemote =
            (getRepoRemote repo)
              { getRemoteName = "origin"
              }
        }

getCredential :: String -> IO String
getCredential host = do
  output <-
    readProcess "git" ["credential", "fill"] $
      intercalate "\n" ["protocol=https", "host=" ++ host, ""]
  pure
    . head
    . map (drop . length $ "password=")
    . filter (isPrefixOf "password=")
    . lines
    $ output
