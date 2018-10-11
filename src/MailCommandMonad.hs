{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

module MailCommandMonad
( CommandContext
, MailCommand
, getServerState
, setServerState
, sendResponse
, sendUntaggedData
, runMailCommand
) where
    
import qualified Data.ByteString as B

import Data.Void

import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Trans

import ConnectionContext

import Control.Applicative (Alternative)

import ServerState

import Logger

import qualified Text.Megaparsec as M
    
data CommandContext = CommandContext
                    { commandTag   :: B.ByteString
                    , connection   :: Connection
                    }

type ParserT = M.ParsecT Void B.ByteString

newtype MailCommand a = MailCommand { unwrap :: ReaderT CommandContext (ParserT (StateT ServerState IO)) a }
                      deriving(Functor, Applicative, Monad, Alternative, MonadPlus)

instance M.MonadParsec Void B.ByteString MailCommand where
    failure i si = MailCommand $ M.failure i si
    fancyFailure s = MailCommand $ M.fancyFailure s
    label s (MailCommand m)  = MailCommand $ M.label s m
    hidden (MailCommand m)  = MailCommand $ M.hidden m
    try (MailCommand m)  = MailCommand $ M.try m
    lookAhead (MailCommand m)  = MailCommand $ M.lookAhead m
    notFollowedBy (MailCommand m)  = MailCommand $ M.notFollowedBy m
    withRecovery f (MailCommand m)  = MailCommand $ M.withRecovery (unwrap . f) m
    observing (MailCommand m)  = MailCommand $ M.observing m
    eof = MailCommand M.eof
    token f s = MailCommand $ M.token f s
    tokens f t = MailCommand $ M.tokens f t
    takeWhileP n p = MailCommand $ M.takeWhileP n p
    takeWhile1P n p  = MailCommand $ M.takeWhile1P n p
    takeP n c = MailCommand $ M.takeP n c
    getParserState = MailCommand M.getParserState
    updateParserState f = MailCommand $ M.updateParserState f


getServerState :: MailCommand ServerState
getServerState = MailCommand get

setServerState :: ServerState -> MailCommand ()
setServerState = MailCommand . put

runMailCommand :: MailCommand a
               -> B.ByteString
               -> Connection
               -> B.ByteString
               -> StateT ServerState IO (Either String a)

runMailCommand (MailCommand m ) commandTag connection unparsedArgs = do
    let parser = runReaderT m (CommandContext commandTag connection)
    parseRes <- M.runParserT parser "Command" unparsedArgs
    case parseRes of
        Left err -> return $ Left (show err)
        Right v  -> return $ Right v


sendUntaggedData :: B.ByteString -> MailCommand ()
sendUntaggedData bs = MailCommand $ do
    conn <- reader connection
    liftIO $ send conn (bs `B.append` B.pack [ 0x0d, 0x0a ])

sendResponse :: B.ByteString -> MailCommand ()
sendResponse respData = MailCommand $ do
    (CommandContext tag conn) <- ask
    let response = tag `B.append` B.cons 0x20 respData `B.append` B.pack [ 0x0d, 0x0a ]
    liftIO $ send conn response

isConnectionSecure :: MailCommand Bool
isConnectionSecure = MailCommand $ reader (isTls . connection)

