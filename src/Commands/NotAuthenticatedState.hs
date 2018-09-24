module Commands.NotAuthenticatedState where

import MailCommandMonad
import Command


notAuthenticatedStateCommands = map (makeCommand notAuthenticatedState)
                              [
                              ]
