{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Project.Config
  ( Config(..)
  , loadConfig
  ) where

import Data.Aeson
import Data.ByteString.Lazy as Lazy
import GHC.Generics

data Config = Config
  { bitbucketUsername :: String
  , bitbucketPassword :: String
  , gitlabToken :: String
  , directory :: FilePath
  } deriving (Generic, Show, FromJSON)

loadConfig :: FilePath -> IO Config
loadConfig path = do
  content <- Lazy.readFile path
  either fail return (eitherDecode content)