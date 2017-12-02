module Project.Exception
  ( JsonException(..)
  ) where

import Control.Exception
import Data.Typeable

newtype JsonException = JsonException
  { message :: String
  } deriving (Show, Typeable)

instance Exception JsonException