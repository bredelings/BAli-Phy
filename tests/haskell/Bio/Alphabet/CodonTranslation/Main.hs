{-# LANGUAGE NoImplicitPrelude #-}

import Bio.Alphabet (alphabetSize, dna, findLetter, getAminoAcids, missingCharIndex, mkCodons, sequenceToText,
                     standard_code, translate)
import Compiler.Num
import Data.Eq ((==))
import Data.Function (($))
import qualified Data.Text as Text
import Foreign.Vector (listToVector)
import System.IO (print)

-- Select generated codon classes after the ordinary codons, verifying their names before testing
-- ordinary, uniformly ambiguous, and heterogeneously ambiguous translations.
main = do
    let codons = mkCodons dna standard_code
        lysine = findLetter (getAminoAcids codons) "K"
        firstClass = alphabetSize codons
        aaa = findLetter codons "AAA"
        aar = firstClass + 1
        aan = firstClass + 10
        className index = sequenceToText codons (listToVector [index])
    print [className aar == Text.pack "AAR", className aan == Text.pack "AAN",
           translate codons aaa == lysine, translate codons aar == lysine,
           translate codons aan == missingCharIndex]
