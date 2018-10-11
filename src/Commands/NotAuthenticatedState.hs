{-# LANGUAGE OverloadedStrings #-}
module Commands.NotAuthenticatedState where
    
import MailCommandMonad
import Command
import ArgumentsParserUtils

import qualified Data.Text.Encoding as TE


notAuthenticatedStateCommands = map (makeCommand notAuthenticatedState)
                              [
                              ]
