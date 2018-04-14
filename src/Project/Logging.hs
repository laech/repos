{-# LANGUAGE FlexibleContexts #-}
module Project.Logging
  ( setupLogger
  , debug
  , info
  ) where

import Control.Monad.Base
import System.Console.ANSI
import System.IO
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import System.Log.Logger

setupLogger :: IO ()
setupLogger = do
  handler <- flip setFormatter ansi <$> streamHandler stdout DEBUG
  updateGlobalLogger rootLoggerName (setLevel DEBUG . setHandlers [handler])

ansi :: LogFormatter a
ansi _ (priority, message) _ =
  pure $
  case priority of
    DEBUG -> setColor Vivid Black message
    INFO -> setColor Dull Blue message
    ERROR -> setColor Dull Red message
    _ -> message
  where
    setColor intensity color message =
      let prefix = setSGRCode [SetColor Foreground intensity color]
          suffix = setSGRCode [Reset]
       in prefix ++ message ++ suffix

info :: MonadBase IO m => String -> String -> m ()
info name message = liftBase $ infoM name message

debug :: MonadBase IO m => String -> String -> m ()
debug name message = liftBase $ debugM name message
