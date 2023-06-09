{-# LANGUAGE NoImplicitPrelude #-}
module Data.Text.IO where

import Compiler.Base  -- for String
import Compiler.IO    -- for IO
import Control.Monad  -- for IO
import Foreign.String -- for CPPString
import Data.Text      -- for Text
import Data.Functor   -- for fmap
import Data.Function  -- for $
import System.IO (FilePath, Handle, IOMode(..), stdin, stdout, openFile, hPutChar, hClose)

readFile :: FilePath -> IO Text
readFile path = do handle <- openFile path ReadMode
                   text <- hGetContents handle
                   hClose handle
                   return text

writeFile :: FilePath -> Text -> IO ()
writeFile path text = do handle <- openFile path WriteMode
                         putStr text
                         hClose handle

appendFile :: FilePath -> Text -> IO ()
appendFile path text = do handle <- openFile path AppendMode
                          putStr text
                          hClose handle

foreign import bpcall "File:" hGetContentsRaw :: Handle -> RealWorld -> CPPString
hGetContents :: Handle -> IO Text
hGetContents h = fmap Text $ makeIO $ hGetContentsRaw h

-- hGetChunk :: Handle -> IO Text

foreign import bpcall "File:" hGetLineRaw :: Handle -> RealWorld -> CPPString
hGetLine :: Handle -> IO Text
hGetLine h = fmap Text $ makeIO $ hGetLineRaw h

foreign import bpcall "File:" hPutStrRaw :: Handle -> CPPString -> RealWorld -> ()
hPutStr :: Handle -> Text -> IO ()
hPutStr h (Text s) = makeIO $ hPutStrRaw h s

hPutStrLn :: Handle -> Text -> IO ()
hPutStrLn h s = hPutStr h s >> hPutChar h '\n'

interact :: (Text -> Text) -> IO ()
interact f = do contents <- getContents
                putStr (f contents)

getContents :: IO Text
getContents = hGetContents stdin

getLine :: IO Text
getLine = hGetLine stdin

putStr :: Text -> IO ()
putStr s = hPutStr stdout s

putStrLn :: Text -> IO () 
putStrLn s = hPutStrLn stdout s
