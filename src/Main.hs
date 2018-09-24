{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Main where
    
import Network.Socket hiding(recv)
import Network.Socket.ByteString

import Control.Exception (bracket)
import Logger

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B


import CommandRegistry
import ConnectionState
import RawCommandParser
import MailCommandMonad
import Command

import Control.Monad.State.Strict

import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Error as M

resolve :: HostName -> ServiceName -> IO AddrInfo
resolve address port = do
    let hints = defaultHints {
          addrFlags = [AI_PASSIVE]
        , addrSocketType = Stream
        }
    addr:_ <- getAddrInfo (Just hints) (Just address) (Just port)
    return addr

open :: AddrInfo -> IO Socket
open addr = do
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    -- Useful during testing, to avoid "already bound address" errors
    setSocketOption sock ReuseAddr 1
    bind sock (addrAddress addr)
    listen sock 10
    return sock



main :: IO ()
main = withSocketsDo $ do

    let registry = makeCommandRegistry
    
    bindAddress <- resolve "127.0.0.1" "8000"
    bracket (open bindAddress) close $ \sock -> do
        (conn, peer) <- accept sock
        logInfo $ "Connection from: " ++ show peer
        startSession conn registry

sendGreeting :: Socket -> IO ()
sendGreeting conn = do
    let greetingMsg = TE.encodeUtf8 "* OK Welcome\r\n"
    sendAll conn greetingMsg

sendBye :: Socket -> IO ()
sendBye conn = do
    let byeMsg = TE.encodeUtf8 "* BYE bb\r\n"
    sendAll conn byeMsg



-- Server logic
startSession :: Socket -> CommandRegistry -> IO ()
startSession conn registry = do
    sendGreeting conn
    runStateT (serverMain conn registry) NotAuthenticated
    return ()


messageReaderLoop :: (MonadState ConnectionState m, MonadIO m) => Socket -> (B.ByteString -> m ()) -> m ()
messageReaderLoop conn action = loop mempty
    where loop leftovers = do
              (msg, leftovers') <- liftIO $ getNextMessage leftovers conn
              action msg
              newState <- get
              case newState of
                  Logout -> return ()
                  _      -> loop leftovers'


serverMain :: Socket -> CommandRegistry -> StateT ConnectionState IO ()
serverMain conn registry = messageReaderLoop conn $ \msg -> do
    case parseRawCommand msg of
        Left err         -> do
            logInfo $ "Parse error: " ++ err
            lift $ reportParseError conn

        Right rawCommand -> do
            let commandTag          = rawCommandTag rawCommand
            let unparsedCommandArgs = rawCommandArguments rawCommand

            case lookupCommand registry (rawCommandName rawCommand) of
                Nothing      -> lift $ reportUnknownCommand conn rawCommand
                Just command -> do
                    currConnectionState <- get
                    if not (isCommandAvaible currConnectionState command)
                    then lift $ reportWrongState conn rawCommand
                    else do
                        logInfo $ "Running command: " ++ show (commandName command)
                        res <- runMailCommand (commandAction command)
                                              commandTag
                                              conn
                                              unparsedCommandArgs

                        case res of
                            Left err -> do
                                logInfo $ "Arguments parse error: " ++ err
                                lift $ reportArgumentParseError conn rawCommand
                            Right _  -> return ()



reportParseError :: Socket -> IO ()
reportParseError conn = do
    let errorMsg = TE.encodeUtf8 $ "* BAD Error while parsing message\r\n"
    sendAll conn errorMsg


reportUnknownCommand :: Socket -> RawCommand -> IO ()
reportUnknownCommand conn command = do
    let errorMsg = (rawCommandTag command) `B.append` TE.encodeUtf8 " BAD unknown command\r\n"
    sendAll conn errorMsg

reportArgumentParseError :: Socket -> RawCommand -> IO ()
reportArgumentParseError conn command = do
    let errorMsg = (rawCommandTag command) `B.append` TE.encodeUtf8 " BAD invalid arguments\r\n"
    sendAll conn errorMsg

reportWrongState :: Socket -> RawCommand -> IO ()
reportWrongState conn command = do
    let errorMsg = (rawCommandTag command) `B.append` TE.encodeUtf8 " BAD invalid command in this state\r\n"
    sendAll conn errorMsg
