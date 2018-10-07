{-# LANGUAGE OverloadedStrings #-}
module RawCommandParser where
    
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Error as M
import qualified Data.ByteString as B

import qualified Text.Megaparsec.Byte as M

import Data.Word (Word8)

import Data.Char (ord)

import Data.Void

import Data.Functor (($>))


import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL

import Network.Socket hiding (recv)
import Network.Socket.ByteString

type Parser = M.Parsec Void B.ByteString

-- Utility that helps you write ascii codepoints using char literals
-- but watch out, the character must be in the range 0 .. 127
ascii :: Char -> Word8
ascii = fromIntegral . ord

isControlChar :: Word8 -> Bool
isControlChar c = (0 <= c && c <= 0x1F) || c == 0x7F

isRespSpecial = (== ascii ']')

isListWildcard = let values = map ascii [ '%', '*' ]
                 in (`elem` values)

isQuotedSpecial = let values = map ascii ['"', '\\' ]
                  in (`elem` values)

isAtomSpecial :: Word8 -> Bool
isAtomSpecial c = isSeparator c ||
                  isControlChar c ||
                  isListWildcard c ||
                  isQuotedSpecial c ||
                  isRespSpecial c

    where isSeparator c  = c `elem` map ascii [ '(' , ')' , '{' , ' ' ]

isAtomChar :: Word8 -> Bool
isAtomChar = not . isAtomSpecial

isAStringChar :: Word8 -> Bool
isAStringChar c = isAtomChar c || isRespSpecial c


tag :: Parser B.ByteString
tag = M.takeWhile1P Nothing (\c -> isAStringChar c && c /= ascii '+')

space :: Parser Word8
space = M.char 0x20

-- This datatype represents a semiparsed command.
-- It is the result of parsing only command tag and command name.
-- Command arguments are left unparsed for further processing by specialized parsers
data RawCommand = RawCommand
                { rawCommandTag       :: B.ByteString
                , rawCommandName      :: B.ByteString
                , rawCommandArguments :: B.ByteString
                }

-- Superficially parse a command.
-- This means just parse tag and command name,
-- but leave arguments unparsed for further processing
rawCommand :: Parser RawCommand
rawCommand = do
    commandTag <- tag
    space
    commandName <- M.takeWhile1P Nothing (/= ascii ' ')
    isAtEnd <- M.atEnd
    if isAtEnd
    then return $ RawCommand commandTag commandName B.empty
    else do
        space
        RawCommand commandTag commandName <$> M.takeRest

parseRawCommand :: B.ByteString -> Either String RawCommand
parseRawCommand input = case M.parse rawCommand "" input of
    Left err -> Left (M.parseErrorPretty err)
    Right res -> Right res

getNextMessage :: B.Builder -> Socket -> IO (B.ByteString, B.Builder)
getNextMessage leftovers conn = do
    let bufferSize = 2048
    chunk <- recv conn bufferSize
    if B.null chunk
    then
        return (BL.toStrict . B.toLazyByteString $ leftovers, mempty)

    else do
        let (head, tail) = B.breakSubstring "\r\n" chunk
        if B.null tail
        then  getNextMessage (leftovers <> B.byteString head) conn
        else do
            let newLeftovers = B.byteString $ B.drop 2 tail
            let result       = BL.toStrict . B.toLazyByteString $ leftovers <> B.byteString head
            return (result, newLeftovers)

