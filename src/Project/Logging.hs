module Project.Logging
  ( setupLogger
  ) where

import System.Console.ANSI
import System.IO
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import System.Log.Logger

setupLogger :: IO ()
setupLogger = do
  handler <- flip setFormatter ansiFormatter <$> streamHandler stdout DEBUG
  updateGlobalLogger rootLoggerName (setLevel DEBUG . setHandlers [handler])

ansiFormatter :: LogFormatter a
ansiFormatter _ (priority, message) _ =
  pure $
  case priority of
    DEBUG -> setColor Vivid Black message
    INFO -> setColor Dull Blue message
    ERROR -> setColor Dull Red message
    _ -> message
  where
    setColor intensity color message =
      setSGRCode [SetColor Foreground intensity color] ++
      message ++ setSGRCode [Reset]