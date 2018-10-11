{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Main where
    
import Network.Socket hiding(recv)
import Network.Socket.ByteString

import Control.Exception (bracket)
import Logger

import CommandRegistry
import ServerLogic
import Tls
import Options.Applicative as Opt

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


data Options = Options
             { useTls :: Bool
             , certificateFilePath :: FilePath
             , privateKeyFilePath  :: FilePath
             , port                :: Int
             }

optionParser :: Opt.Parser Options
optionParser = Options
             <$> switch
                 (  long "tls"
                 <> help "Start connection using TLS"
                 )
             <*> strOption
                 ( long "cert"
                 <> help "SSL Certificate filepath"
                 <> metavar "FILEPATH"
                 )
             <*> strOption
                 ( long "privkey"
                 <> help "SSL Certificate private key filepath"
                 <> metavar "FILEPATH"
                 )
             <*> option auto
                 ( long "port"
                 <> short 'p'
                 <> value 8000
                 <> help "Port on which to listen"
                 <> showDefault
                 <> metavar "PORT"
                 )

progInfo = info (optionParser <**> helper)
         ( fullDesc
        <> header "piccione - an imap server for testing purposes"
        )

main :: IO ()
main = execParser progInfo >>= \opts -> withSocketsDo $ do

    let registry = makeCommandRegistry

    tlsParams <- loadTlsSettings (certificateFilePath opts) (privateKeyFilePath opts)
    case tlsParams of
        Nothing -> return ()
        Just tlsParams -> do
            bindAddress <- resolve "127.0.0.1" "8000"
            bracket (open bindAddress) close $ \sock -> do
                (conn, peer) <- accept sock
                logInfo $ "Connection from: " ++ show peer
                serverMain conn (useTls opts) tlsParams registry

