{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Project.HTTP where

import Control.Exception (Exception, throwIO)
import Data.Aeson (FromJSON)
import Network.HTTP.Client (Manager, Request, responseBody)
import Pipes.Aeson (decode)
import Pipes.HTTP (withHTTP)
import Pipes.Parse (evalStateT)

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
