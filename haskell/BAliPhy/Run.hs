{-# LANGUAGE NoImplicitPrelude #-}
module BAliPhy.Run
    ( prepareOutputFiles
    ) where

import Compiler.Base
import Control.Monad
import Data.Bool
import Data.Function
import Data.Functor
import Data.List
import Foreign.String
import Foreign.Vector
import System.Exit
import System.IO

foreign import bpcall "File:"
    reserveOutputFilesRaw :: EVector CPPString -> IO (EVector CPPString)

-- Claim every logger path before any logger can truncate it, or return all paths already present.
reserveOutputFiles :: [FilePath] -> IO [FilePath]
reserveOutputFiles filenames = do
    collisions <- reserveOutputFilesRaw (toVector [pack_cpp_string filename | filename <- filenames])
    return [unpack_cpp_string filename | filename <- vectorToList collisions]

-- Explicit overwrite leaves logger opening unchanged; otherwise the native operation claims
-- the entire destination set before this function allows model construction to continue.
prepareOutputFiles :: Bool -> [FilePath] -> IO ()
prepareOutputFiles True _ = return ()
prepareOutputFiles False filenames = do
    collisions <- reserveOutputFiles filenames
    case collisions of
        [] -> return ()
        _ -> do
            hPutStrLn stderr "Refusing to overwrite existing BAli-Phy output files:"
            mapM_ (\filename -> hPutStrLn stderr $ "  " ++ filename) collisions
            hPutStrLn stderr "Choose another directory with --output-dir, or pass --overwrite."
            exitFailure
