{-# LANGUAGE OverloadedStrings #-}
module ServerLogic (serverMain) where
    
import Network.Socket
import qualified Network.TLS as TLS

import CommandRegistry
import MailCommandMonad
import Command
import ConnectionContext
import RawCommandParser
import ServerState
import MessageReader
import Logger

import qualified Data.Text.Encoding as TE

import Control.Monad.State.Strict

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B

serverMain :: Socket -> Bool -> TLS.ServerParams -> CommandRegistry -> IO ()
serverMain conn useTlsConnection tlsParams registry = do
    let ctx = plainConnection conn
    let action = if useTlsConnection then
                    startTls ctx tlsParams $ \conn -> do
                        logInfo "Successfully started TLS connection."
                        serverLoop registry tlsParams conn
                 else
                    serverLoop registry tlsParams ctx

    evalStateT action NotAuthenticated

type ServerMonad = StateT ServerState IO

serverLoop :: CommandRegistry -> TLS.ServerParams -> Connection -> ServerMonad ()
serverLoop registry tlsParams conn = do
    logInfo "Session started."
    liftIO $ sendGreeting conn
    evalStateT (loop registry tlsParams conn) mempty


loop :: CommandRegistry -> TLS.ServerParams -> Connection -> MessageReaderT ServerMonad ()
loop registry tlsParams conn = do
        msg <- nextMessage conn
        case parseRawCommand msg of
            Left err -> do
                logInfo $ "Parse error: " ++ err
                reportParseError conn

            Right rawCommand -> handleRawCommand rawCommand


    where handleRawCommand rawCommand =
              case rawCommandName rawCommand of
                  "LOGOUT" -> do
                      logInfo "Running command: LOGOUT"
                      doLogout rawCommand
                  "STARTTLS" -> do
                      logInfo "Running command: STARTTLS"
                      -- If the connection is already in tls it does nothing,
                      -- just sends ok response
                      doStartTls rawCommand

                  _  -> case lookupCommand registry (rawCommandName rawCommand) of
                          Nothing -> lift $ reportUnknownCommand conn rawCommand
                          Just command -> handleCommand rawCommand command



          handleCommand rawCommand command = do
              currServerState <- lift get -- get from underlying server monad
              if not (isCommandAvaible currServerState command) then
                  reportWrongState conn rawCommand
              else do
                  logInfo $ "Running command: " ++ show (commandName command)
                  res <- lift $ runMailCommand (commandAction command)
                                               (rawCommandTag rawCommand)
                                               conn
                                               (rawCommandArguments rawCommand)

                  case res of
                      Left err -> do
                          logInfo $ "Arguments parse error: " ++ err
                          reportArgumentParseError conn rawCommand
                      _ -> return ()

                  loop registry tlsParams conn


          doLogout :: RawCommand -> MessageReaderT ServerMonad ()
          doLogout rawCommand =
              -- Enforce that there must be no argument
              if B.null (rawCommandArguments rawCommand) then do
                  sendBye conn
                  let msg = rawCommandTag rawCommand `B.append` TE.encodeUtf8 " OK Closing now.\r\n"
                  liftIO $ ConnectionContext.send conn msg
              else do
                  logInfo "Arguments parse error: Arguments were provided, but I expected none"
                  reportArgumentParseError conn rawCommand
                  -- TODO: Probabily even if if there is a failure in the command parsing
                  -- we should stop the server and quit
                  loop registry tlsParams conn

          doStartTls :: RawCommand -> MessageReaderT ServerMonad ()
          doStartTls rawCommand =
              -- Enforce that there must be no argument
              if B.null (rawCommandArguments rawCommand) then do
                  let msg = rawCommandTag rawCommand `B.append` TE.encodeUtf8 " OK Start Handshake now.\r\n"
                  liftIO $ ConnectionContext.send conn msg
                  startTls conn tlsParams $ \conn -> do
                      logInfo "Successfully started TLS connection."
                      loop registry tlsParams conn
              else do
                  logInfo "Arguments parse error: Arguments were provided, but I expected none"
                  reportArgumentParseError conn rawCommand
                  loop registry tlsParams conn

sendGreeting :: MonadIO m => Connection -> m ()
sendGreeting conn = do
    let greetingMsg = TE.encodeUtf8 "* OK Welcome\r\n"
    liftIO $ ConnectionContext.send conn greetingMsg

sendBye :: MonadIO m => Connection -> m ()
sendBye conn = do
    let byeMsg = TE.encodeUtf8 "* BYE bb\r\n"
    liftIO $ ConnectionContext.send conn byeMsg

reportParseError :: MonadIO m => Connection -> m ()
reportParseError conn = do
    let errorMsg = TE.encodeUtf8 "* BAD Error while parsing message\r\n"
    liftIO $ ConnectionContext.send conn errorMsg


reportUnknownCommand :: MonadIO m => Connection -> RawCommand -> m ()
reportUnknownCommand conn command = do
    let errorMsg = rawCommandTag command `B.append` TE.encodeUtf8 " BAD unknown command\r\n"
    liftIO $ ConnectionContext.send conn errorMsg

reportArgumentParseError :: MonadIO m => Connection -> RawCommand -> m ()
reportArgumentParseError conn command = do
    let errorMsg = rawCommandTag command `B.append` TE.encodeUtf8 " BAD invalid arguments\r\n"
    liftIO $ ConnectionContext.send conn errorMsg

reportWrongState :: MonadIO m => Connection -> RawCommand -> m ()
reportWrongState conn command = do
    let errorMsg = rawCommandTag command `B.append` TE.encodeUtf8 " BAD invalid command in this state\r\n"
    liftIO $ ConnectionContext.send conn errorMsg
