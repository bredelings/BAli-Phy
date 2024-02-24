module Effect where

import Control.Monad

-- An Effect may be a node in a graph??
data Effect

foreign import bpcall "Modifiables:" getProperties :: a -> b

{-
 For effects, such as registering a prior, we need to be able to
 avoid them when merely computing quantities of interest.

 For example, when computing conditional likelihoods, we don't want
 to force the alignment prior, which can happen since the CLs access
 pairwise alignments inside the AlignmentOnTree object.

 This is for two reasons:
 (1) Computing the alignment prior wastes time.
 (2) The alignment is actually not connected, and the current value of
     some of the pairwise alignments is integer 0, indicating "unset".

 QUESTION: how is withEffect different than `seq`?
 ANSWER: in (withEffect x y), y is a separate node?

 QUESTION: why is this a problem?

bali-phy: Error! evaluating reg # 8240 (unchangeable): case <18762> of {_ -> <18763>}

evaluating reg # 18762 (unchangeable): case <18763> of {_ -> <7239>}

evaluating reg # 18763 (unchangeable): MCMC:runMCMC Main.#201670 0 <7239>

evaluating reg # 13705 (unchangeable): case <12507> of {_ -> <12508>}

evaluating reg # 12507 (unchangeable): case <12508> of {_ -> <2404>}

evaluating reg # 12508 (unchangeable): MCMC:sample_SPR_all <7113> 0 Compiler.IO.#25831

evaluating reg # 13195: Likelihood:peelBranchTowardRoot <11041> <11042> <11043> <11044> <15614>

evaluating reg # 10972: makeEVector

evaluating reg # 13197: Likelihood:peelBranchTowardRoot <19510> <19509> <19508> <19507> <15614>

evaluating reg # 19508: IntMap:restrictKeysToVector <15615> <19511>

evaluating reg # 15615: case <15781> of {Bio.Alignment.AlignmentOnTree b c d e -> e}

evaluating reg # 15781: case <12657> of {(c, d) -> case c of {_ -> case d of {_ -> <12658>}}}

evaluating reg # 12657: case <12650> of {(c, d) -> case c of {_ -> let {f = case e of {Compiler.IO.IO g -> g}} in f c}}

evaluating reg # 12650: case <6513> of {(c, d) -> case c of {_ -> let {f = case e of {Compiler.IO.IO g -> g}} in f c}}

evaluating reg # 6513: case <5529> of {(c, d) -> case c of {_ -> let {f = case e of {Compiler.IO.IO g -> g}} in f c}}

evaluating reg # 5529: case <5347> of {(c, d) -> case c of {_ -> let {f = case e of {Compiler.IO.IO g -> g}} in f c}}

evaluating reg # 5347: case <5165> of {(c, d) -> case c of {_ -> let {f = case e of {Compiler.IO.IO g -> g}} in f c}}

evaluating reg # 5165: case <20959> of {(c, d) -> case c of {_ -> let {f = case e of {Compiler.IO.IO g -> g}} in f c}}

evaluating reg # 20959: case <20806> of {(c, d) -> case c of {_ -> let {f = case e of {Compiler.IO.IO g -> g}} in f c}}

evaluating reg # 20806: case <20637> of {(c, d) -> case c of {_ -> let {f = case e of {Compiler.IO.IO g -> g}} in f c}}

evaluating reg # 20637: case <20458> of {(c, d) -> case c of {_ -> let {f = case e of {Compiler.IO.IO g -> g}} in f c}}

evaluating reg # 20458: <20457> <20600>

evaluating reg # 7854: case <12254> of {_ -> let {c = case <12307> of {Compiler.IO.IO d -> d}} in c <12254>}

evaluating reg # 12254: case <12130> of {_ -> <20600>}

evaluating reg # 12130: Modifiables:register_prior <12637> <12143> <20600>

evaluating reg # 12143: Num:multiply_logdouble <12589> <12589>

evaluating reg # 12589: Prelude:divide_logdouble Probability.Distribution.PhyloAlignment.$v#121457 <12194>

evaluating reg # 12194: Prelude:doubleToLogDouble <12310>

evaluating reg # 12310: Alignment:rs07_lengthp <12497> <12311>

evaluating reg # 12311: Alignment:pairwise_alignment_length1 <19543>

Treating '0' as object type!

So, 12254 = seq <12130> <20600> ends up forcing <12130>.
Why doesn't this happen with `withEffect`?

-}

foreign import bpcall "Modifiables:" withEffect :: a -> b -> b

--- An effect monad that doesn't require sequencing of its operations.

data Effect2 a = Effect2 { runEffect :: () -> a }

instance Functor Effect2 where
    fmap f x = Effect2 (\_ -> let y = runEffect x () in y `seq` f y)

instance Applicative Effect2 where
    pure x = Effect2 (\_ -> x)
    t1 <*> t2 = Effect2 (\_ -> let f = runEffect t1 ()
                                   x = runEffect t2 ()
                               in f `seq` x `seq` f x)

instance Monad Effect2 where
    f >>= g = Effect2 (\_ -> let x = runEffect f () in x `seq` runEffect (g x) ())

