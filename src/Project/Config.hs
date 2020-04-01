{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Project.Config
  ( Config (..),
    loadConfig,
  )
where

import Control.Exception
import Data.Aeson
import Data.ByteString.Lazy as L
import Data.Typeable
import GHC.Generics

newtype ConfigException = ConfigException String
  deriving (Show, Typeable)

instance Exception ConfigException

data Config
  = Config
      { gitlabToken :: String,
        directory :: FilePath
      }
  deriving (Generic, Show, FromJSON)

loadConfig :: FilePath -> IO Config
loadConfig path = L.readFile path >>= parse
  where
    parse content = either failed pure (eitherDecode content)
    failed = throwIO . ConfigException
