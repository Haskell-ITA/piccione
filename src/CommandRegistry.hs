module CommandRegistry
( CommandRegistry
, makeCommandRegistry
, lookupCommand
) where

import qualified Data.Map.Strict as M

import qualified Data.ByteString as B

import Command

import Commands.AnyState
import Commands.NotAuthenticatedState
import Commands.AuthenticatedState
import Commands.SelectedState

newtype CommandRegistry = CommandRegistry { unwrap :: M.Map B.ByteString Command }

makeRegistry :: [Command] -> CommandRegistry
makeRegistry = CommandRegistry . M.fromList . map (\cmd -> (commandName cmd, cmd))


lookupCommand :: CommandRegistry -> B.ByteString -> Maybe Command
lookupCommand (CommandRegistry reg) name = M.lookup name reg


makeCommandRegistry = makeRegistry $ anyStateCommands
                                   ++ notAuthenticatedStateCommands
                                   ++ authenticatedStateCommands
                                   ++ selectedStateCommands
