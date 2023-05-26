{-# LANGUAGE NoImplicitPrelude #-}
module System.IO (module System.IO,
                  module Compiler.IO)
    where

import Compiler.Base -- for String
import Compiler.IO -- for String

type FilePath = String

data Handle

{-

-- input, output, or both
-- open, closed, or semi-closed
-- seekable or not
-- buffering is enabled, disabled, enabled by line, enabled by block
-- a buffer

stdin :: Handle

stdout :: Handle

stderr :: Handle

withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r

openFile :: FilePath -> IOMode -> IO Handle

data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode

-- deriving Eq, Show, Read, Ord, Enum

hClose :: Handle -> IO ()

readFile :: FilePath -> IO String

-- strict
readFile' :: FilePath -> IO String

appendFile :: FilePath -> String -> IO ()

hFileSize :: Handle -> IO Integer

hSetFileSize :: Handle -> Integer -> IO ()

hIsEOF :: Handle -> IO Bool

isEOF = IO Bool
isEOF = hIsEOF stdin

data BufferMode = NoBuffering | LineBuffering | BlockBuffering (Maybe Int)

hSetBuffering :: Handle -> BufferMode -> IO ()

hFlush :: Handle -> IO ()

hGetPosn :: Handle -> IO HandlePosn

hSetPosn :: HandlePsn -> IO ()

data HandlePosn

hSeek :: Handle -> SeekMode -> Integer -> IO ()

data SeekMode = AbsoluteSeek | RelativeSeek | SeekFromEnd

hTell :: Handle -> IO Integer

hIsOpen :: Handle -> IO Bool

hIsClosed :: Handle -> IO Bool

hIsReadable :: Handle -> IO Bool

hIsWriteable :: Handle -> IO Bool

hIsSeekable :: Handle -> IO Bool

-- Not portable
               
hIsTerminalDevice :: Handle -> IO Bool

hSetEcho :: Handle -> Bool -> IO ()

hGetEcho :: Handle -> IO Bool

hShow :: Handle -> IO String

hWaitForInput :: Handle -> Int -> IO Bool

hReady :: Handle -> IO Bool

hGetChar :: Handle -> IO Char

hGetLine :: Handle -> IO String

hLookAhead :: Handle -> IO Char

hGetContents :: Handle -> IO String

-- strict
hGetContents' :: Handle -> IO String

hPutChar :: Handle -> Char -> IO ()

hPutStr :: Handle -> String -> IO ()

hPutStrLn :: Handle -> String -> IO ()

hPrint :: Show a => Handle -> a -> IO ()
hPrint x = hPutStr (show x)

interact :: (String -> String) -> IO ()

putChar :: Char -> IO ()
putChar c = hPutChar stdout c

putStr :: String -> IO ()
putStr s = hPutStr stdout s

putStrLn :: String -> IO ()
putStrLn s = hPutStr stdout s

print :: Show a => a -> IO ()
print x = putStr (show x)

getChar :: IO Char
getChar = hGetChar stdin

getLine :: IO String
getLine = hGetLine stdin

getContents :: IO String
getContents = hGetContents stdin

getContents' :: IO String -- strict
getContents' = hGetContents' stdin

readIO :: Read a => String -> IO a
-- catches exceptions and calls fail in the IO monad

readLn :: Read a => IO a
readLn = getLine >>= readIO

withBinaryFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r

openBinaryFile :: FilePath -> IOMode -> IO Handle

hSetBinaryMode :: Handle -> Bool -> IO ()

data Ptr a

hPutBuf :: Handle -> Ptr a -> Int -> IO ()

hGetBuf :: Handle -> Ptr a -> Int -> IO Int

hGetBufSome :: Handle -> Ptr a -> Int -> IO Int

hPutBufNonBlocking :: Handle -> Ptr a -> Int -> IO ()

hGetBufNonBlocking :: Handle -> Ptr a -> Int -> IO Int

openTempFile :: FilePath -> String -> IO (FilePath, Handle)

openBinaryTempFile :: FilePath -> String -> IO (FilePath, Handle)

openTempFileWithDefaultPermissions :: FilePath -> String -> IO (FilePath, Handle)

openBinaryTempFileWithDefaultPermissions :: FilePath -> String -> IO (FilePath, Handle)

hSetEncoding :: Handle -> TextEncoding -> IO ()

hGetEncoding :: Handle -> IO (Maybe TextEncoding)

data TextEncoding

latin1 :: TextEncoding

utf8 :: TextEncoding

utf8_bom :: TextEncoding

utf16 :: TextEncoding

utf16le :: TextEncoding         

utf16be :: TextEncoding         

utf32 :: TextEncoding

utf32le :: TextEncoding

utf32be :: TextEncoding

char8 :: TextEncoding

mkTextEncoding :: String -> IO TextEncoding

hSetNewlineMode :: Handle -> NewlineMode -> IO ()

data Newline = LF | CRLF

nativeNewline :: Newline -- LF on Unix, CRLF on Windows

data NewlineMode = NewlineMode { inputNL, outputNL :: Newline }

noNewlineTranslation :: NewlineMode
noNewlineTranslation = NewlineMode { inputNL = LF, outputNL = LF }
                        
universalNewlineMode :: NewlineMode
universalNewlineMode  = NewlineMode { inputNL = CRLF, outputNL = nativeNewLine }

nativeNewlineMode :: NewlineMode
nativeNewlineMode = NewlineMode { inputNL = nativeNewline, outputNL = nativeNewline }
-}
