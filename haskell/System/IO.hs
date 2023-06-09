{-# LANGUAGE NoImplicitPrelude #-}
module System.IO (module System.IO,
                  module Compiler.IO)
    where

import Compiler.Base -- for String
import Compiler.IO -- for String
import Data.Bool
import Data.Maybe
import Foreign.String
import Text.Show -- for Show
import Text.Read -- for Read
import Control.Monad -- for >>
import Data.Functor -- for fmap
import Data.Function -- for $
import Compiler.Enum

import Data.Exception

type FilePath = String

data Handle
{-
-- input, output, or both
-- open, closed, or semi-closed
-- seekable or not
-- buffering is enabled, disabled, enabled by line, enabled by block (with block size)
-- a buffer
-}


data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
-- deriving Eq, Show, Read, Ord, Enum

intFromIOMode ReadMode = 0
intFromIOMode WriteMode = 1
intFromIOMode AppendMode = 2
intFromIOMode ReadWriteMode = 3

foreign import bpcall "File:" getStdin :: () -> Handle

foreign import bpcall "File:" getStdout :: () -> Handle

foreign import bpcall "File:" getStderr :: () -> Handle

stdin :: Handle
stdin = getStdin ()

stdout :: Handle
stdout = getStdout ()

stderr :: Handle
stderr = getStderr ()

withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFile path mode action = do
  handle <- openFile path mode
  action handle

foreign import bpcall "File:" openFileRaw :: CPPString -> Int -> RealWorld -> Handle
openFile :: FilePath -> IOMode -> IO Handle
openFile path mode = makeIO $ openFileRaw (list_to_string path) (intFromIOMode mode)


foreign import bpcall "File:" hCloseRaw :: Handle -> RealWorld -> ()

hClose :: Handle -> IO ()
hClose handle = makeIO $ hCloseRaw handle

readFile :: FilePath -> IO String
readFile path = do handle <- openFile path ReadMode
                   text <- hGetContents handle
                   hClose handle
                   return text

-- strict
readFile' :: FilePath -> IO String
readFile' path = do handle <- openFile path ReadMode
                    text <- hGetContents' handle
                    hClose handle
                    return text

writeFile :: FilePath -> String -> IO ()
writeFile path text = do handle <- openFile path WriteMode
                         putStr text
                         hClose handle

appendFile :: FilePath -> String -> IO ()
appendFile path text = do handle <- openFile path AppendMode
                          putStr text
                          hClose handle

-- These are apparently for when we know the file that the handle is attached to.
-- But what if the file has been unliked from the filesystem?
-- Then we'd have to own the dentry or something...
foreign import bpcall "File:" hFileSizeRaw :: Handle -> RealWorld -> Integer
hFileSize :: Handle -> IO Integer
hFileSize h = makeIO $ hFileSizeRaw h

{-
hSetFileSize :: Handle -> Integer -> IO ()
-}

foreign import bpcall "File:" hIsEOFRaw :: Handle -> RealWorld -> Bool
hIsEOF :: Handle -> IO Bool
hIsEOF h = makeIO $ hIsEOFRaw h

isEOF :: IO Bool
isEOF = hIsEOF stdin

data BufferMode = NoBuffering | LineBuffering | BlockBuffering (Maybe Int)

{-

hSetBuffering :: Handle -> BufferMode -> IO ()
-}

foreign import bpcall "File:" hFlushRaw :: Handle -> RealWorld -> ()
hFlush :: Handle -> IO ()
hFlush h = makeIO $ hFlushRaw h

{-
hGetPosn :: Handle -> IO HandlePosn

hSetPosn :: HandlePsn -> IO ()
-}

data HandlePosn

data SeekMode = AbsoluteSeek | RelativeSeek | SeekFromEnd
intFromSeekMode AbsoluteSeek = 0
intFromSeekMode RelativeSeek = 1
intFromSeekMode SeekFromEnd  = 2

foreign import bpcall "File:" hSeekRaw :: Handle -> Int -> Integer -> RealWorld -> ()
hSeek :: Handle -> SeekMode -> Integer -> IO ()
hSeek h mode pos = makeIO $ hSeekRaw h (intFromSeekMode mode) pos

{-
C++ streams can have different read and write positions: tellg() vs tellp()
Not sure what to do here.

hTell :: Handle -> IO Integer
-}

foreign import bpcall "File:" hIsOpenRaw :: Handle -> RealWorld -> Bool
hIsOpen :: Handle -> IO Bool
hIsOpen h = makeIO $ hIsOpenRaw h

hIsClosed :: Handle -> IO Bool
hIsClosed h = fmap not $ hIsOpen h

{-
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
-}

foreign import bpcall "File:" hGetCharRaw :: Handle -> RealWorld -> Char
hGetChar :: Handle -> IO Char
hGetChar h = makeIO $ hGetCharRaw h

foreign import bpcall "File:" hGetLineRaw :: Handle -> RealWorld -> CPPString
hGetLine :: Handle -> IO String
hGetLine h = fmap unpack_cpp_string $ makeIO $ hGetLineRaw h

foreign import bpcall "File:" hLookAheadRaw :: Handle -> RealWorld -> Char
hLookAhead :: Handle -> IO Char
hLookAhead h = makeIO $ hLookAheadRaw h

foreign import bpcall "File:" hGetContentsRaw :: Handle -> RealWorld -> CPPString
hGetContents :: Handle -> IO String
hGetContents h = unsafeInterleaveIO $ hGetContents' h

-- strict
hGetContents' :: Handle -> IO String
hGetContents' h = fmap unpack_cpp_string $ makeIO $ hGetContentsRaw h

foreign import bpcall "File:" hPutCharRaw :: Handle -> Char -> RealWorld -> ()
hPutChar :: Handle -> Char -> IO ()
hPutChar h c = makeIO $ hPutCharRaw h c

foreign import bpcall "File:" hPutStrRaw :: Handle -> CPPString -> RealWorld -> ()
hPutStr :: Handle -> String -> IO ()
hPutStr h s = makeIO $ hPutStrRaw h $ list_to_string s

hPutStrLn :: Handle -> String -> IO ()
hPutStrLn h s = hPutStr h s >> hPutChar h '\n'

hPrint :: Show a => Handle -> a -> IO ()
hPrint h x = hPutStr h (show x)


interact :: (String -> String) -> IO ()
interact f = do contents <- getContents
                putStr (f contents)

putChar :: Char -> IO ()
putChar c = hPutChar stdout c

putStr :: String -> IO ()
putStr s = hPutStr stdout s

putStrLn :: String -> IO ()
putStrLn s = hPutStrLn stdout s

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
readIO s = catch (return $ read s) (\e -> fail "readIO failed")
-- catches exceptions and calls fail in the IO monad

readLn :: Read a => IO a
readLn = getLine >>= readIO


withBinaryFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withBinaryFile path mode action = do
  handle <- openBinaryFile path mode
  action handle

foreign import bpcall "File:" openBinaryFileRaw :: CPPString -> Int -> RealWorld -> Handle
openBinaryFile :: FilePath -> IOMode -> IO Handle
openBinaryFile path mode = makeIO $ openBinaryFileRaw (list_to_string path) (intFromIOMode mode)

{-
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

-}

data TextEncoding

{-

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

-}

data Newline = LF | CRLF

{-
nativeNewline :: Newline -- LF on Unix, CRLF on Windows
-}

data NewlineMode = NewlineMode { inputNL, outputNL :: Newline }

{-
noNewlineTranslation :: NewlineMode
noNewlineTranslation = NewlineMode { inputNL = LF, outputNL = LF }
                        
universalNewlineMode :: NewlineMode
universalNewlineMode  = NewlineMode { inputNL = CRLF, outputNL = nativeNewLine }

nativeNewlineMode :: NewlineMode
nativeNewlineMode = NewlineMode { inputNL = nativeNewline, outputNL = nativeNewline }
-}
