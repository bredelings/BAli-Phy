module Bio.Sequence where

import Data.Map as Map hiding (map)
import Bio.Alphabet
import Compiler.FFI.Import (CInput)
import Data.Text (Text)
import qualified Data.Text as T
import Data.BitVector
import Data.Maybe (isJust)
import qualified Data.Vector.Unboxed as U

-- Opaque handle to the C++ `sequence` used while loading sequence files.
data ESequence
instance CInput ESequence

foreign import trcall "Alignment:sequence_name" sequenceName :: ESequence -> Text
foreign import trcall "Alignment:" sequenceDataRaw :: ESequence -> Text

type Sequence = (Text,Text)

-- Map Text Text would be nicer, but discards the order of the sequences.
type Sequences = [Sequence]

mkSequence :: ESequence -> Sequence
mkSequence s = (sequenceName s, sequenceDataRaw s)

-- FIXME: make these operate on just the text, not the pair?
-- FIXME: remove sequence_to_indices in favor of stripGaps . sequenceToAlignedIndices?
foreign import bpcall "Alignment:sequence_to_indices" builtin_sequence_to_indices :: Alphabet -> CPPString -> EVector Int
foreign import trcall "Alignment:sequenceToAlignedIndices" sequenceToAlignedIndices :: Alphabet -> Text -> EVector Int
sequence_to_indices a (_, s) = builtin_sequence_to_indices a (T.toCppString s)

-- sequence_to_indices :: Sequence -> [Int]
-- maybe add this later

foreign import trcall "Alignment:statesToLetters" statesToLetters :: EVector Int -> U.Vector Int -> EVector Int

foreign import trcall "Alignment:loadSequences" loadSequencesRaw :: String -> IO (EVector ESequence)
loadSequences :: String -> IO [Sequence]
loadSequences filename = fmap (fmap mkSequence . vectorToList) $ loadSequencesRaw filename

foreign import trcall "Alignment:getRange" getRange :: String -> Int -> EVector Int
foreign import trcall "Alignment:" selectRangeRaw :: EVector Int -> Text -> Text
selectRange :: String -> [Sequence] -> [Sequence]
selectRange range sequences = let maxLength = maximum [ T.length $ snd s | s <- sequences ]
                                  range' = getRange range maxLength
                                  select (name, chars) = (name, selectRangeRaw range' chars)
                               in fmap select sequences

reorderSequences names sequences | length names /= length sequences  = error "Sequences.reorderSequences: different number of names and sequences!"
                                  | otherwise = [ sequences_map Map.! name | name <- names ]
    where sequences_map = Map.fromList [ (fst sequence, sequence) | sequence <- sequences ]

getSequenceLengths sequenceData = Map.fromList [ (label, vector_size isequence) | (label, isequence) <- getSequences $ sequenceData]

foreign import trcall "Likelihood:" bitmaskFromSequence :: EVector Int -> CBitVector
foreign import trcall "Likelihood:" stripGaps :: EVector Int -> EVector Int
foreign import trcall "Likelihood:" maskSequenceRaw :: CBitVector -> EVector Int -> EVector Int

bitmaskFromSequence' s = BitVector $ bitmaskFromSequence s
maskSequence (BitVector bv) sequence = maskSequenceRaw bv sequence

fastaSeq (label, seq) = T.concat [T.singleton '>', label, T.singleton '\n', seq, T.singleton '\n']

fastaSeqs sequences = T.concat [fastaSeq s | s <- sequences]

{- NOTE: If we switch to multiple alphabet types.

data CharacterData = forall a.Alphabet a => CharacterData a [(Text, EVector Int)]
   OR
data CharacterData a = CharacterData a [(Text, EVector Int)]

If we switch to multiple Alphabet types, we would probably need to have a main
class Alphabet and then also a class Nucleotides and a class Triplets.

* If we used `forall a.Alphabet a =>` then that only packages the methods for the
  main class.

* If we used `CharacterData a`, then we know what the underlying type is, but we
  can't put them in a list.

* If we use Data.Dynamic for the alphabet, then we could check if its a Codons DNA,
  but I don't know if can check if its a `Codons a` while finding out the a.

-}

data CharacterData = CharacterData Alphabet [(Text, EVector Int)]
newtype AlignedCharacterData = Aligned CharacterData
newtype UnalignedCharacterData = Unaligned CharacterData

instance HasAlphabet CharacterData where
    getAlphabet (CharacterData a _) = a

instance HasAlphabet AlignedCharacterData where
    getAlphabet (Aligned d) = getAlphabet d

instance HasAlphabet UnalignedCharacterData where
    getAlphabet (Unaligned d) = getAlphabet d

class HasSequences d where
    -- If we change the sequences to observations, then does this generalization still work?
    getSequences :: d -> [(Text, EVector Int)]

instance HasSequences CharacterData where
    getSequences (CharacterData _ d) = d

instance HasSequences AlignedCharacterData where
    getSequences (Aligned d) = getSequences d

instance HasSequences UnalignedCharacterData where
    getSequences (Unaligned d) = getSequences d

getTaxa d = map fst $ getSequences d


mkCharacterData :: Alphabet -> Sequences -> CharacterData
mkCharacterData alphabet sequences = CharacterData alphabet [(label, go sequence) | (label, sequence) <- sequences]
    where go = sequenceToAlignedIndices alphabet

mkUnalignedCharacterData alphabet sequences = Unaligned (CharacterData alphabet indices')
    where CharacterData _ indices = mkCharacterData alphabet sequences
          indices' = map (\(label,is) -> (label, stripGaps is)) indices

allSameAs x xs = and ((x==) <$> xs)

allSame []                       = error "allSame: nothing to compare!"
allSame (x:xs) | allSameAs x xs  = Just x
               | otherwise       = Nothing

checkSameLengths d@(CharacterData _ sequences) | isJust $ allSame lengths = d
                                               | otherwise                = error "Sequences have different lengths!"
    where lengths = [vector_size x | (_,x) <- sequences]

mkAlignedCharacterData alphabet sequences = Aligned $ checkSameLengths $ mkCharacterData alphabet sequences

unalign (Aligned (CharacterData a sequences)) = Unaligned (CharacterData a [(l, stripGaps s) | (l,s) <- sequences])
