{-# LANGUAGE NoImplicitPrelude #-}

import Bio.Alphabet (dna, findLetter, getAminoAcids, missingCharIndex, mkCodons, sequenceToText,
                     standard_code, translate, translateObservation)
import Bio.Sequence (CharacterData(CharacterData), mkCharacterData)
import Compiler.Num
import Data.Eq ((==))
import Data.Function (($))
import qualified Data.Text as Text
import qualified Data.Vector.Unboxed as U
import System.IO (print)

-- Keep alphabet-level ambiguous translation coverage while obtaining stored observation
-- codes and their shared database through the public character-data encoder.
main = do
    let codons = mkCodons dna standard_code
        lysine = findLetter (getAminoAcids codons) "K"
        aaa = findLetter codons "AAA"
        CharacterData _ ambiguities [(_, encoded)] =
            mkCharacterData codons [(Text.pack "ambiguous", Text.pack "AARAAN")]
        encodedName position = sequenceToText codons ambiguities (U.slice position 1 encoded)
    print [encodedName 0 == Text.pack "AAR", encodedName 1 == Text.pack "AAN",
           translate codons aaa == lysine,
           translateObservation codons ambiguities (encoded U.! 0) == lysine,
           translateObservation codons ambiguities (encoded U.! 1) == missingCharIndex]
