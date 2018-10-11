{-# LANGUAGE ExistentialQuantification #-}
module ConnectionContext
( Connection
, send
, recv
, isTls
, plainConnection
, startTls
) where
    
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Network.Socket as N
import qualified Network.Socket.ByteString as NB

import qualified Network.TLS as TLS

import Control.Monad.Trans

import qualified Data.ByteString.Builder as B

import Control.Monad (unless)

data Connection = forall s. (TLS.HasBackend s) => Connection
                { send :: B.ByteString -> IO ()
                , recv :: IO B.ByteString
                , backend :: s
                , isTls   :: Bool
                }

plainConnection :: N.Socket -> Connection
plainConnection conn = Connection
                     { send = NB.sendAll conn
                     , recv = NB.recv conn 2048
                     , backend = conn
                     , isTls   = False
                     }


makeTlsConnection :: TLS.Context -> Connection
makeTlsConnection tlsCtx = Connection
                     { send = TLS.sendData tlsCtx . BL.fromStrict
                     , recv = TLS.recvData tlsCtx
                     , backend = TLS.ctxConnection tlsCtx
                     , isTls   = True
                     }

-- if the given connection is a plain connection, start a tls session
-- otherwise do nothing
startTls :: MonadIO m => Connection -> TLS.ServerParams -> (Connection -> m ()) -> m ()
startTls (Connection _ _ backend isTls) settings f = unless isTls $ do
    ctx <- TLS.contextNew backend settings
    TLS.handshake ctx
    f (makeTlsConnection ctx)
    TLS.bye ctx

