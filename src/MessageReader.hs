{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module MessageReader (nextMessage, MessageReaderT) where
    
import Control.Monad.State.Strict
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL

import ConnectionContext

type MessageReaderT m a = StateT B.Builder m a


getNextMessage :: B.Builder -> Connection -> IO (B.ByteString, B.Builder)
getNextMessage leftovers conn = do
    chunk <- recv conn
    if B.null chunk then
        return (BL.toStrict . B.toLazyByteString $ leftovers, mempty)
    else do
        let (head, tail) = B.breakSubstring "\r\n" chunk
        if B.null tail then
            getNextMessage (leftovers <> B.byteString head) conn
        else do
            let newLeftovers = B.byteString $ B.drop 2 tail
            let result = BL.toStrict . B.toLazyByteString $ leftovers <> B.byteString head
            return (result, newLeftovers)


nextMessage :: MonadIO m => Connection -> MessageReaderT m B.ByteString
nextMessage conn = do
    leftovers <- get
    (msg, leftovers') <- liftIO $ getNextMessage leftovers conn
    put leftovers'
    return msg

