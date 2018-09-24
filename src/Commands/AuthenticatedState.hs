module Commands.AuthenticatedState where

import MailCommandMonad
import Command


authenticatedStateCommands = map (makeCommand authenticatedState)
                           [
                           ]
