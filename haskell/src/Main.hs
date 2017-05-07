{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Config
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Network.HTTP.Client as HTTP

import Config (Config)
import Control.Exception (Exception, throw)
import Data.Aeson (FromJSON, eitherDecode)
import Data.List (sort)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import System.Directory (doesDirectoryExist)
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitSuccess, ExitFailure), die)
import System.FilePath (FilePath, (</>), takeFileName, dropExtension)
import System.Process (createProcess, waitForProcess, shell, cwd)

data Page = Page { values :: [Repository], next :: Maybe String }
  deriving (Generic, Show)

newtype Repository = Repository { links :: Links }
  deriving (Generic, Show)

newtype Links = Links { clone :: [Clone] }
  deriving (Generic, Show)

data Clone = Clone { name :: String, href :: Url }
  deriving (Generic, Show)

instance FromJSON Page
instance FromJSON Repository
instance FromJSON Links
instance FromJSON Clone

newtype JsonException = JsonException String
  deriving (Typeable, Show)

newtype ProcessException = ProcessException String
  deriving (Typeable, Show)

instance Exception JsonException
instance Exception ProcessException

type Url = String
type User = String
type Pass = String
type PageResult = ([String], Maybe Url)

downloadSshUrls :: Url -> User -> Pass -> HTTP.Manager -> IO [String]
downloadSshUrls url user pass manager = do
  (sshUrls, maybeNextUrl) <- downloadPageResult url user pass manager
  case maybeNextUrl of
    Just nextUrl -> (++ sshUrls) <$> downloadSshUrls nextUrl user pass manager
    _ -> return sshUrls

downloadPageResult :: Url -> User -> Pass -> HTTP.Manager -> IO PageResult
downloadPageResult url user pass manager = do
  request <-
    HTTP.applyBasicAuth (B.pack user) (B.pack pass) <$> HTTP.parseUrlThrow url
  response <- HTTP.httpLbs request manager
  return . parsePageResult . HTTP.responseBody $ response

parsePageResult :: L.ByteString -> PageResult
parsePageResult json =
  case eitherDecode json :: Either String Page of
    Left err -> throw . JsonException $ err ++ "\n" ++ L.unpack json
    Right page -> (getSshUrls page, next page)

getSshUrls :: Page -> [String]
getSshUrls page = sshUrls
  where
    repos = values page
    clones = concatMap (clone . links) repos
    sshClones = filter (\clone -> name clone == "ssh") clones
    sshUrls = map href sshClones

getProjectNameFromSshUrl :: String -> String
getProjectNameFromSshUrl = dropExtension . takeFileName

checkoutRepo :: FilePath -> String -> IO ()
checkoutRepo parentDirectory sshUrl = do
  let directory = parentDirectory </> getProjectNameFromSshUrl sshUrl
  exists <- doesDirectoryExist directory
  (_, _, _, handle) <- createProcess $ if exists
    then (shell "git fetch -v") { cwd = Just directory }
    else (shell $ "git clone -v " ++ sshUrl) { cwd = Just parentDirectory }
  exitCode <- waitForProcess handle
  case exitCode of
    ExitSuccess -> return ()
    _ -> throw (ProcessException (show exitCode))

process :: FilePath -> IO ()
process configFile = do
  config <- Config.load configFile
  manager <- HTTP.newManager tlsManagerSettings

  putStrLn "Fetching project list..."
  sshUrls <-
    sort <$> downloadSshUrls
      ("https://bitbucket.org/api/2.0/repositories/" ++ Config.username config)
      (Config.username config)
      (Config.password config)
      manager

  if null sshUrls
    then putStrLn "No project found."
    else mapM_ putStrLn sshUrls

  mapM_ (checkoutRepo (Config.directory config)) sshUrls

main :: IO ()
main = do
  args <- getArgs
  case args of
    [configFile] -> process configFile
    _ -> die "Usage: <this-program> <config-file>"
