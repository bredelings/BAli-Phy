module Probability.Logger where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO
import qualified Data.JSON as J
import Probability.Random
import Foreign.String as FS
import Foreign.Vector as FV

import Tree
import Tree.Newick

-- addLogger subsample logger obj = addLoggingAction subsample (logAppend logger obj)

{-
The problem with this is that setting
  type LogValue TreeLogger = forall t.(WriteNewickNode (Rooted t)) => t
doesn't work because we can't unify this with something.

Possibly this could be fixed by using this as an expected type and checking the type that
should match, instead of unifying.
-}

class Logger a where
    type LogValue a
    logAppend :: a -> LogValue a -> Int -> LogDouble -> LogDouble -> LogDouble -> IO ()

data JSONLogger = JSONLogger Handle

{-
instance Logger JSONLogger where
    type LogValue JSONLogger = [(J.Key,J.JSON)]
    logAppend (JSONLogger handle) ljson iter = writeJSON handle ljson iter
-}


every n logger iter prior likelihood probability | iter `mod` n == 0  = logger iter prior likelihood probability
                                                 | otherwise          = return ()

----

data ColumnNames

jsonLogger filename = do
  handle <- openFile filename WriteMode
  hPutStrLn handle "{\"fields\":[\"iter\",\"prior\",\"likelihood\",\"posterior\"],\"nested\":true,\"format\":\"MCON\",\"version\":\"0.1\"}"
  hFlush handle
  return $ writeJSON handle


tsvLogger filename firstFields = do
  handle <- openFile filename WriteMode
  stateref <- newIORef Nothing
  return $ writeTSV handle firstFields stateref


-- We might need QuickLook to handle types like IO (forall t. *).
-- treeLogger :: FilePath -> IO ( forall t. (HasRoot (Rooted t), WriteNewickNode (Rooted t), IsTree t) => t -> Int -> IO ())
treeLogger filename = do handle <- openFile filename WriteMode
                         return $ writeTree handle

alignmentLogger filename = do handle <- openFile filename WriteMode
                              return $ writeAlignment handle


writeAlignment file alignment iter _ _ _  = do hPutStrLn file $ "iterations = " ++ show iter
                                               hPutStrLn file ""
                                               T.hPutStrLn file alignment
                                               hPutStrLn file ""
                                               hPutStrLn file ""
                                               hFlush file

-- We need to be operating OUTSIDE the context in order to get the prior, likelihood, and posterior.

writeJSON file ljson iter prior likelihood posterior = do T.hPutStrLn file $
                                                           J.jsonToText $
                                                           J.Object ["iter" %=% iter,
                                                                     "prior" %=% prior,
                                                                     "likelihood" %=% likelihood,
                                                                     "posterior" %=% posterior,
                                                                     "parameters" %>% ljson]
                                                          hFlush file

-- writeTree :: Handle -> (forall t. (Tree t, WriteNewickNode (Rooted t), HasRoot (Rooted t)) => t -> Int -> IO ())
writeTree file tree iter _ _ _ = do T.hPutStrLn file $ writeNewick tree
                                    hFlush file

foreign import bpcall "Foreign:tsvHeaderAndMapping" builtinTsvHeaderAndMapping :: EVector CPPString -> J.CJSON -> EPair CPPString ColumnNames
tsvHeaderAndMapping firstFields csample = let (cstring,mapping) = pair_from_c $ builtinTsvHeaderAndMapping cFirstFields csample
                                              cFirstFields = FV.list_to_vector [ FS.pack_cpp_string field | field <- firstFields ] 
               in (T.fromCppString cstring, mapping)

foreign import bpcall "Foreign:getTsvLine" builtinGetTsvLine :: ColumnNames -> J.CJSON -> CPPString
getTsvLine mapping sample = T.fromCppString $ builtinGetTsvLine mapping sample

writeTSV file firstFields stateref ljson iter prior likelihood posterior = do
  let j = J.Object ["iter" %=% iter,
                    "prior" %=% prior,
                    "likelihood" %=% likelihood,
                    "posterior" %=% posterior,
                    "parameters" %>% ljson]
      cj = J.c_json j

  state <- readIORef stateref

  case state of
    Nothing -> do let (header, mapping) = tsvHeaderAndMapping firstFields cj
                  T.hPutStrLn file $ header
                  writeIORef stateref (Just mapping)
                  T.hPutStrLn file $ getTsvLine mapping cj

    Just mapping -> T.hPutStrLn file $ getTsvLine mapping cj

  hFlush file
