module Tls where

import Control.Monad.Trans

import qualified Network.TLS as TLS
import qualified Network.TLS.Extra as TLSExtra

import Network.Socket
import Logger
import Data.Default

loadTlsSettings :: FilePath -> FilePath -> IO (Maybe TLS.ServerParams)
loadTlsSettings certPath privatekeyPath = do
    cred <- TLS.credentialLoadX509 certPath privatekeyPath
    case cred of
        Left err -> do
            logError $ "Error loading ssl certificates: " ++ err
            return Nothing

        Right cred -> do
           let serverShared = def
                            { TLS.sharedCredentials = TLS.Credentials [cred]
                            }
           let serverSupported = def
                               { TLS.supportedCiphers = TLSExtra.ciphersuite_strong
                               }

           let tlsParams = def
                         { TLS.serverShared = serverShared
                         , TLS.serverSupported = serverSupported
                         }

           return (Just tlsParams)
