{-# LANGUAGE FlexibleContexts #-}

module Project.Logging
  ( setupLogger,
    debug,
    info,
  )
where

import System.Console.ANSI
  ( Color (Black, Blue, Red),
    ColorIntensity (Dull, Vivid),
    ConsoleLayer (Foreground),
    SGR (Reset, SetColor),
    setSGRCode,
  )
import System.IO (stdout)
import System.Log.Formatter (LogFormatter)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (streamHandler)
import System.Log.Logger
  ( Priority (DEBUG, ERROR, INFO),
    debugM,
    infoM,
    rootLoggerName,
    setHandlers,
    setLevel,
    updateGlobalLogger,
  )

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
    setColor intensity color message =
      let prefix = setSGRCode [SetColor Foreground intensity color]
          suffix = setSGRCode [Reset]
       in prefix ++ message ++ suffix

info :: String -> IO ()
info = infoM rootLoggerName

debug :: String -> IO ()
debug = debugM rootLoggerName
