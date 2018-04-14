{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Project.HTTP where

import Control.Exception
import Control.Monad.Base
import Data.Aeson hiding (decode)
import Network.HTTP.Client
import Pipes
import Pipes.Aeson
import Pipes.HTTP
import Pipes.Parse

getJSON ::
     (MonadBase IO m, FromJSON a, Exception e)
  => Request
  -> Manager
  -> (String -> e)
  -> m a
getJSON req man err =
  liftBase $
  withHTTP req man $ \resp ->
    evalStateT decode (responseBody resp) >>= \case
      Nothing -> failed "end of input"
      Just (Left e) -> failed (show e)
      Just (Right r) -> pure r
  where
    failed = throwIO . err
