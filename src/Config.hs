{-# LANGUAGE DeriveGeneric #-}

module Config
  ( Config(..)
  , ConfigException
  , load
  ) where

import qualified Data.ByteString.Lazy as L

import           Control.Exception    (Exception, throwIO)
import           Data.Aeson           (FromJSON, eitherDecode)
import           Data.Typeable        (Typeable)
import           GHC.Generics         (Generic)

data Config = Config
  { bitbucketUsername :: String
  , bitbucketPassword :: String
  , gitlabToken       :: String
  , directory         :: FilePath
  } deriving (Generic, Show)

instance FromJSON Config

newtype ConfigException = ConfigException String
  deriving (Typeable, Show)

instance Exception ConfigException

load :: FilePath -> IO Config
load path = do
  json <- L.readFile path
  case eitherDecode json of
    Left err     -> throwIO (ConfigException err)
    Right config -> return config
