module Project.Logging
  ( setupLogger,
    debug,
    info,
  )
where

import System.Console.ANSI
import System.IO
import System.Log.Formatter
import System.Log.Handler hiding (setLevel)
import System.Log.Handler.Simple
import System.Log.Logger

setupLogger :: IO ()
setupLogger = createHandler >>= updateLogger
  where
    createHandler = flip setFormatter ansi <$> streamHandler stdout DEBUG
    updateLogger handler =
      updateGlobalLogger rootLoggerName (setLevel DEBUG . setHandlers [handler])

ansi :: LogFormatter a
ansi _ (priority, message) _ =
  pure $ case priority of
    DEBUG -> setColor Vivid Black message
    INFO -> setColor Dull Blue message
    ERROR -> setColor Dull Red message
    _ -> message
  where
    setColor intensity color msg =
      let prefix = setSGRCode [SetColor Foreground intensity color]
          suffix = setSGRCode [Reset]
       in prefix ++ msg ++ suffix

info :: String -> IO ()
info = infoM rootLoggerName

debug :: String -> IO ()
debug = debugM rootLoggerName
