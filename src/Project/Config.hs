{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Project.Config
  ( Config(..)
  , loadConfig
  ) where

import Control.Monad.Base
import Data.Aeson
import Data.ByteString.Lazy as Lazy
import GHC.Generics

data Config = Config
  { bitbucketUsername :: String
  , bitbucketPassword :: String
  , gitlabToken :: String
  , directory :: FilePath
  } deriving (Generic, Show, FromJSON)

loadConfig :: MonadBase IO m => FilePath -> m Config
loadConfig path = do
  content <- liftBase $ Lazy.readFile path
  either fail return (eitherDecode content)
