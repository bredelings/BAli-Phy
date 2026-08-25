{-# LANGUAGE NoImplicitPrelude #-}

import Bio.Alphabet (alphabetSize, dna, findLetter, getAminoAcids, missingCharIndex, mkCodons, sequenceToText,
                     standard_code, translate)
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
        firstClass = alphabetSize codons
        aaa = findLetter codons "AAA"
        aar = firstClass + 1
        aan = firstClass + 10
        CharacterData _ ambiguities [(_, encoded)] =
            mkCharacterData codons [(Text.pack "ambiguous", Text.pack "AARAAN")]
        encodedName position = sequenceToText codons ambiguities (U.slice position 1 encoded)
    print [encodedName 0 == Text.pack "AAR", encodedName 1 == Text.pack "AAN",
           translate codons aaa == lysine, translate codons aar == lysine,
           translate codons aan == missingCharIndex]
