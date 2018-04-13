{-# LANGUAGE LambdaCase #-}

module Project.HTTP where

import Control.Exception
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson hiding (decode)
import Network.HTTP.Client
import Pipes
import Pipes.Aeson
import Pipes.HTTP
import Pipes.Parse

getJSON
  :: (MonadIO m, FromJSON a, Exception e)
  => Request -> Manager -> (String -> e) -> m a
getJSON req man err =
  liftIO $
  withHTTP req man $ \resp ->
    evalStateT decode (responseBody resp) >>= \case
      Nothing -> failed "end of input"
      Just (Left e) -> failed (show e)
      Just (Right r) -> pure r
  where
    failed = throwIO . err
