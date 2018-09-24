module Commands.SelectedState where

import MailCommandMonad
import Command


selectedStateCommands = map (makeCommand selectedState)
                      [
                      ]
