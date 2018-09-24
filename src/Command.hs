module Command
( CommandStateMask
, anyState
, notAuthenticatedState
, authenticatedState
, selectedState
, CommandDef (..)
, Command
, commandName
, commandAction
, isCommandAvaible
, makeCommand
) where

import ConnectionState

import Data.Bits
import Data.Int (Int8)

import qualified Data.ByteString as B

import MailCommandMonad

newtype CommandStateMask = CommandStateMask { unwrap :: Int8 }


-- AnyState < *
-- NotAuthenticated
-- Authenticated < Selected

stateMask :: ConnectionState -> Int8
stateMask NotAuthenticated = 1 -- 001
stateMask Authenticated    = 2 -- 010
stateMask Selected         = 6 -- 110      When in Selected state we are also in Authenticated state
stateMask Logout           = 0 -- 000      No command is ammisible in this state

anyState              = CommandStateMask maxBound       -- 111
notAuthenticatedState = CommandStateMask 1              -- 001
authenticatedState    = CommandStateMask 2              -- 010
selectedState         = CommandStateMask 4              -- 100



isCommandAvaible' :: ConnectionState -> CommandStateMask -> Bool
isCommandAvaible' state commandMask = (stateMask state .&. unwrap commandMask) /= 0

data Command = Command
             { commandStateMask :: CommandStateMask
             , def       :: CommandDef
             }

data CommandDef = CommandDef
             { name             :: B.ByteString
             , action           :: MailCommand ()
             }

commandName :: Command -> B.ByteString
commandName = name . def

commandAction :: Command -> MailCommand ()
commandAction = action . def

isCommandAvaible :: ConnectionState -> Command -> Bool
isCommandAvaible state cmd = isCommandAvaible' state (commandStateMask cmd)

makeCommand :: CommandStateMask -> CommandDef -> Command
makeCommand mask def = Command mask def
