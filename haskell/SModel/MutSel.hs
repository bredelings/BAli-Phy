module SModel.MutSel where

import Foreign.Vector
import SModel.ReversibleMarkov
import SModel.Codons
import SModel.Frequency -- for get_ordered_elements

foreign import bpcall "SModel:" mut_sel_q :: Matrix Double -> EVector Double -> Matrix Double
foreign import bpcall "SModel:" mut_sel_pi :: EVector Double -> EVector Double -> EVector Double

mut_sel w' (ReversibleMarkov a smap q0 pi0 _ _) = reversible_markov a smap q pi where
    w = list_to_vector w'
    q = mut_sel_q q0 w
    pi = mut_sel_pi pi0 w

mut_sel' w' q0 = mut_sel w q0 where
    w = get_ordered_elements (letters a) w' "fitnesses"
    a = getAlphabet q0

mut_sel_aa ws q@(ReversibleMarkov codon_a _ _ _ _ _) = mut_sel (aa_to_codon codon_a ws) q

mut_sel_aa' ws' q0 = mut_sel_aa ws q0 where
    ws = get_ordered_elements (letters amino_alphabet) ws' "fitnesses"
    codon_alphabet = getAlphabet q0
    amino_alphabet = getAminoAcids codon_alphabet

fMutSel codon_a codon_w omega nuc_model = nuc_model & x3 codon_a & dNdS omega & mut_sel codon_w

fMutSel' codon_a codon_ws' omega nuc_model = fMutSel codon_a codon_ws omega nuc_model
    where codon_ws = get_ordered_elements (letters codon_a) codon_ws' "fitnesses"

aa_to_codon codon_a xs = [xs_array!aa | codon <- codons, let aa = translate codon_a codon]
    where xs_array = listArray' xs
          codons = take n_letters [0..]
          n_letters = alphabetSize codon_a

-- \#1->let {w' = listAray' #1} in \#2 #3->fMutSel #0 codon_w #2 #3
-- The whole body of the function is let-floated up in round 2, and w' is eliminated.
fMutSel0 codon_a aa_w omega nuc_q  = fMutSel codon_a codon_w omega nuc_q
    where codon_w = aa_to_codon codon_a aa_w

fMutSel0' codon_a amino_ws' omega nuc_model = fMutSel0 codon_a amino_ws omega nuc_model
                                               where amino_ws = get_ordered_elements (letters amino_a) amino_ws' "fitnesses"
                                                     amino_a = getAminoAcids codon_a

-- Issue: bad mixing on fMutSel model

    
