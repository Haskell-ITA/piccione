{-# LANGUAGE OverloadedStrings #-}
module Commands.AnyState where
    
import MailCommandMonad
import Command
import ConnectionState

import qualified Data.Text.Encoding as TE
import qualified Data.Text as T

import qualified Text.Megaparsec as M

import ArgumentsParserUtils

anyStateCommands = map (makeCommand anyState)
                 [ noop
                 , logout
                 ]

noop = CommandDef
     { name   = "NOOP"
     , action = noArguments *> sendResponse (TE.encodeUtf8 "OK funziona")
     }

logout = CommandDef
        { name = "LOGOUT"
        , action = do
            noArguments
            sendUntaggedData (TE.encodeUtf8 "* BYE Server is closing.")
            sendResponse (TE.encodeUtf8 "OK Closing now.")
            setConnectionState Logout
        }
