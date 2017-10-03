module Main where

import           Bitbucket
import           Config
import           Data.Either
import           Git
import           Gitlab
import           System.Environment
import           System.Exit
import           System.IO

main :: IO ()
main = do
  args <- getArgs
  case args of
    [configFile] -> process configFile >>= exitWith
    _            -> die "Usage: <this-program> <config-file>"

process :: FilePath -> IO ExitCode
process path = do
  config <- Config.load path
  results <- getSshUrls config >>= printErrors
  let allUrls = foldl1 (++) . map getOrEmpty $ results
  exitCode <- fetchRepos (directory config) allUrls
  return $
    case exitCode of
      ExitSuccess ->
        if any isLeft results
          then ExitFailure 1
          else ExitSuccess
      _ -> exitCode

getOrEmpty :: Either a [b] -> [b]
getOrEmpty x =
  case x of
    Right val -> val
    _         -> []

getSshUrls :: Config -> IO [Either String [String]]
getSshUrls config =
  sequence
    [ getGitlabRepoSshUrls (gitlabToken config)
    , getBitbucketRepoSshUrls
        (bitbucketUsername config)
        (bitbucketPassword config)
    ]

printErrors :: [Either String a] -> IO [Either String a]
printErrors xs = mapM_ (hPutStrLn stderr) (lefts xs) >> return xs
