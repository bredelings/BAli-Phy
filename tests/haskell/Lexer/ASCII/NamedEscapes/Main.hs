{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Data.Char (ord)
import System.IO (putStrLn)
import Text.Show (show)

main = do
  putStrLn (show
    [ ord '\NUL', ord '\SOH', ord '\STX', ord '\ETX', ord '\EOT', ord '\ENQ', ord '\ACK', ord '\BEL'
    , ord '\BS', ord '\HT', ord '\LF', ord '\VT', ord '\FF', ord '\CR', ord '\SO', ord '\SI'
    , ord '\DLE', ord '\DC1', ord '\DC2', ord '\DC3', ord '\DC4', ord '\NAK', ord '\SYN', ord '\ETB'
    , ord '\CAN', ord '\EM', ord '\SUB', ord '\ESC', ord '\FS', ord '\GS', ord '\RS', ord '\US'
    , ord '\SP', ord '\DEL'
    ])
  putStrLn (show "\ESC[1;34m")
