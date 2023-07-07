module Probability.Logger where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO
import qualified Data.JSON as J
import Probability.Random

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
    logAppend :: a -> LogValue a -> Int -> IO ()

data JSONLogger = JSONLogger Handle

instance Logger JSONLogger where
    type LogValue JSONLogger = [(J.Key,J.JSON)]
    logAppend (JSONLogger handle) ljson iter = writeJSON handle ljson iter


every n logger iter | iter `mod` n == 0  = logger iter
                    | otherwise          = return ()

----

jsonLogger filename = do
  handle <- openFile "C1.log.json" WriteMode
  hPutStrLn handle "{\"fields\":[\"iter\",\"prior\",\"likelihood\",\"posterior\"],\"nested\":true,\"version\":\"0.1\"}"
  return $ writeJSON handle

treeLogger filename = do handle <- openFile filename WriteMode
                         return $ writeTree handle

alignmentLogger filename = do handle <- openFile filename WriteMode
                              return $ writeAlignment handle


writeAlignment file alignment iter = do hPutStrLn file $ "iterations = " ++ show iter
                                        hPutStrLn file ""
                                        T.hPutStrLn file alignment
                                        hPutStrLn file ""
                                        hPutStrLn file ""

-- We need to be operating OUTSIDE the context in order to get the prior, likelihood, and posterior.

writeJSON file ljson iter = do T.hPutStrLn file (J.jsonToText $ J.Object ["iter" %=% iter,
                                                                         -- prior
                                                                         -- likelihood
                                                                         -- posterior 
                                                                         "parameters" %>% ljson])

writeTree file tree iter = T.hPutStrLn file $ write_newick $ addInternalLabels tree
