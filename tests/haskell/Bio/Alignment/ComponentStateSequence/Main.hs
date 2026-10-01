{-# LANGUAGE NoImplicitPrelude #-}

import Bio.Alignment (ComponentStateSequence(ComponentStateSequence),
                      componentStates, fixedLeafColumns, fixedLeafStateEncoding,
                      mkExactCharacterData)
import Bio.Alphabet (dna)
import Compiler.Num
import qualified Data.IntMap as IntMap
import qualified Data.JSON as JSON
import qualified Data.Text as T
import qualified Data.Text.IO as Text
import qualified Data.Vector.Unboxed as U
import System.IO (print)
import Tree (getNodesSet)
import Tree.Newick (newickToTree, parse_newick)

-- Full-program tests do not inspect catStates precisely, so check offset views, O(1) projection,
-- and that fixed-alignment gaps remain internal but are omitted from ComponentStateSequence JSON.
-- The JSON check is obsolete if character-property output stops using ungapped coordinates.
main = do
    let components = U.slice 1 3
            (U.fromList [99,-1,2,3,99] :: U.Vector Int)
        states = U.slice 2 3
            (U.fromList [99,99,-1,4,5,99] :: U.Vector Int)
        values = U.zip components states
        sequence = ComponentStateSequence values
    print sequence
    print (U.toList (componentStates sequence))
    Text.putStrLn (JSON.encode sequence)
    Text.putStrLn (JSON.encode (ComponentStateSequence (U.empty :: U.Vector (Int,Int))))

    -- Check output against observations even when gap columns contain auxiliary sampled states.
    -- This extends the encoding test because MCMC smoke tests do not check exact retained values;
    -- it becomes obsolete if fixed-alignment property output stops using observed-leaf coordinates.
    (tree, _) <- newickToTree (parse_newick "(a,b,c)ancestor;")
    let observations = mkExactCharacterData dna
            [(T.pack "a", U.fromList [0,-1,-3,-2]),
             (T.pack "b", U.fromList [0,1,2,3]),
             (T.pack "c", U.fromList [-1,-3,-1,-3])]
        sampled = ComponentStateSequence (U.zip
            (U.slice 1 4 (U.fromList [99,10,11,12,13,99]))
            (U.slice 2 4 (U.fromList [99,99,0,1,2,3,99])))
        allStates = IntMap.fromSet (\_ -> sampled) (getNodesSet tree)
    Text.putStrLn (JSON.fromEncoding
        (fixedLeafStateEncoding (fixedLeafColumns observations) tree allStates))
