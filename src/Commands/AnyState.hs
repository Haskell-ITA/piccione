{-# LANGUAGE OverloadedStrings #-}
module Commands.AnyState where
    
import MailCommandMonad
import Command

import qualified Data.Text.Encoding as TE
import qualified Data.Text as T

import qualified Text.Megaparsec as M

import ArgumentsParserUtils

anyStateCommands = map (makeCommand anyState)
                 [ noop
                 ]

noop = CommandDef
     { name   = "NOOP"
     , action = noArguments *> sendResponse (TE.encodeUtf8 "OK funziona")
     }
