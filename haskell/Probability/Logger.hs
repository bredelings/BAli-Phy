{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Probability.Logger where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO
import Data.JSON as J
import qualified Data.JSON.Encoding as JE
import Probability.Random
import Foreign.String as FS
import Foreign.Vector as FV

import Tree
import Tree.Newick

import Data.Semigroup ((<>))
import MCMC.Loggers (LoggerAction)
import MCMC.Types (runContextAction)

emptyContextObject = J.toCJSON (J.Object [])

emptyContextFields _ _ = return emptyContextObject

plainLogger logSample = (emptyContextFields, \iteration _ -> logSample iteration)

-- Apply the sampling condition to both phases so skipped context fields are not computed.
every n (getContextFields, logSample) = (getContextFieldsEvery, logSampleEvery)
  where
    getContextFieldsEvery iteration context
      | iteration `mod` n == 0 = getContextFields iteration context
      | otherwise              = return emptyContextObject
    logSampleEvery iteration contextFields
      | iteration `mod` n == 0 = logSample iteration contextFields
      | otherwise              = return ()

----

data ColumnNames

jsonLogger :: FilePath -> IO (LoggerValues -> LoggerAction)
jsonLogger filename = do
  handle <- openFile filename WriteMode
  hPutStrLn handle $
    "{\"fields\":[\"iter\",\"prior\",\"likelihood\",\"posterior\"],\"nested\":true," ++
    "\"format\":\"MCON\",\"version\":\"0.2\"}"
  hFlush handle
  return $ makeJSONLogger handle

ejsonLogger filename = do
  handle <- openFile filename WriteMode
  return $ writeJSONe handle

tsvLogger :: FilePath -> [String] -> IO (LoggerValues -> LoggerAction)
tsvLogger filename firstFields = do
  handle <- openFile filename WriteMode
  stateref <- newIORef Nothing
  return $ makeTSVLogger handle firstFields stateref


-- We might need QuickLook to handle types like IO (forall t. *).
-- treeLogger :: FilePath -> IO ( forall t. (HasRoot (Rooted t), WriteNewickNode (Rooted t), IsTree t) => t -> Int -> IO ())
treeLogger filename = do handle <- openFile filename WriteMode
                         return $ writeTree handle

alignmentLogger filename = do handle <- openFile filename WriteMode
                              return $ writeAlignment handle


-- Record one alignment sample using the existing human-readable layout.
writeAlignment file alignment = plainLogger $ \iter -> do
  hPutStrLn file $ "iterations = " ++ show iter
  hPutStrLn file ""
  T.hPutStrLn file alignment
  hPutStrLn file ""
  hPutStrLn file ""
  hFlush file

-- Compute the context-specific part of one scalar log record.
evaluateContextFields action _ context = do
  values <- runContextAction action context
  return $ J.toCJSON $ J.Object values

writeJSONe :: Handle -> Series -> LoggerAction
writeJSONe file fields = plainLogger $ \iter -> do
  T.hPutStrLn file $ J.fromEncoding $ pairs ("iter" .= iter <> fields)
  hFlush file

makeJSONLogger :: Handle -> LoggerValues -> LoggerAction
makeJSONLogger file loggerValues =
  ( evaluateContextFields (contextLogValues loggerValues)
  , logJSONSample file (parameterLogValues loggerValues)
  )

-- Evaluate parameter fields and emit one physical MCON JSON record.
logJSONSample file parameters iter contextFields = do
  T.hPutStrLn file $ J.fromEncoding encoding
  hFlush file
  where
    encoding = pairs ("iter" .= iter <>
                      JE.pair "statistics//" (J.cjsonToEncoding contextFields) <>
                      JE.pair "parameters//" (pairs $ toSeries parameters))

-- writeTree :: Handle -> (forall t. (Tree t, WriteNewickNode (Rooted t), HasRoot (Rooted t)) => t -> Int -> IO ())
writeTree file tree = plainLogger $ \_ -> do
  T.hPutStrLn file $ writeNewick tree
  hFlush file

foreign import bpcall "Foreign:tsvHeaderAndMapping" builtinTsvHeaderAndMapping :: EVector CPPString -> J.CJSON -> EPair CPPString ColumnNames
tsvHeaderAndMapping firstFields csample = let (cstring,mapping) = pair_from_c $ builtinTsvHeaderAndMapping cFirstFields csample
                                              cFirstFields = FV.toVector [ FS.pack_cpp_string field | field <- firstFields ] 
               in (T.fromCppString cstring, mapping)

foreign import bpcall "Foreign:getTsvLine" builtinGetTsvLine :: ColumnNames -> J.CJSON -> CPPString
getTsvLine mapping sample = T.fromCppString $ builtinGetTsvLine mapping sample

foreign import bpcall "Foreign:makeScalarLogRecord" makeScalarLogRecord
  :: Int -> J.CJSON -> J.CJSON -> J.CJSON

makeTSVLogger :: Handle -> [String] -> IORef (Maybe ColumnNames) -> LoggerValues -> LoggerAction
makeTSVLogger file firstFields stateref loggerValues =
  ( evaluateContextFields (contextLogValues loggerValues)
  , logTSVSample file firstFields stateref (parameterLogValues loggerValues)
  )

-- Evaluate parameter fields and emit one row using the cached TSV column mapping.
logTSVSample file firstFields stateref parameters iter contextFields = do
  let parameterFields = J.toCJSON $ J.Object parameters
      sample = makeScalarLogRecord iter contextFields parameterFields

  state <- readIORef stateref

  case state of
    Nothing -> do let (header, mapping) = tsvHeaderAndMapping firstFields sample
                  T.hPutStrLn file $ header
                  writeIORef stateref (Just mapping)
                  T.hPutStrLn file $ getTsvLine mapping sample

    Just mapping -> T.hPutStrLn file $ getTsvLine mapping sample

  hFlush file
