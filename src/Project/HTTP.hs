{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Project.HTTP where

import Control.Exception
import Data.Aeson hiding (decode)
import Network.HTTP.Client
import Pipes.Aeson
import Pipes.HTTP
import Pipes.Parse

getJSON ::
  (FromJSON a, Exception e) =>
  (String -> e) ->
  Manager ->
  Request ->
  IO a
getJSON err man req =
  withHTTP req man $ \resp ->
    evalStateT decode (responseBody resp) >>= \case
      Nothing -> failed "end of input"
      Just (Left e) -> failed (show e)
      Just (Right r) -> pure r
  where
    failed = throwIO . err
