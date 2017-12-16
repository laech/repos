{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Project.Config
  ( Config(..)
  , ConfigException
  , loadConfig
  ) where

import Data.ByteString.Lazy as LazyByteString

import Control.Exception
import Data.Aeson
import Data.Typeable
import GHC.Generics

data Config = Config
  { bitbucketUsername :: String
  , bitbucketPassword :: String
  , gitlabToken :: String
  , directory :: FilePath
  } deriving (Generic, Show, FromJSON)

newtype ConfigException =
  ConfigException String
  deriving (Typeable, Show)

instance Exception ConfigException

loadConfig :: FilePath -> IO Config
loadConfig path =
  LazyByteString.readFile path >>= \json ->
    case eitherDecode json of
      Left err -> throwIO (ConfigException err)
      Right config -> return config