module Logger where

import Control.Monad.Trans

-- Dead simple logger

data Severity = Info
              | Warning
              | Error

logMessage :: MonadIO m => Severity -> String -> m ()
logMessage Info msg    = liftIO . putStrLn $ "LOG INFO: " ++ msg
logMessage Warning msg = liftIO . putStrLn $ "LOG WARN: " ++ msg
logMessage Error msg   = liftIO . putStrLn $ "LOG ERR: " ++ msg


logInfo, logWarning, logError :: MonadIO m => String -> m ()
logInfo    = logMessage Info
logWarning = logMessage Warning
logError   = logMessage Error
