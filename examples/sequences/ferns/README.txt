1. Convert orig.nex -> orig.fasta
  I used seaview
2. Remove a sequence from orig.fasta -> cleaned.fasta

  // I did this
  Remove sequence with seaview -> cleaned.fasta
  sed -i 's/\?/N/g' cleaned.fasta

  // FIXME! Removes empty columns
  alignment-thin --remove Cystopters_fragilis_5950_c8_592 orig.fasta | sed 's/\?/N/g' > cleaned.fasta
  // FIXME! alignment-info also empty columns

3. Select exon/intron pieces from alignment with gaps used to separate them.

  alignment-cat -c 5-8       -e cleaned.fasta > exon1.fasta
  alignment-cat -c 9-453     -e cleaned.fasta > intron1.fasta
  alignment-cat -c 454-596   -e cleaned.fasta > exon2.fasta
  alignment-cat -c 597-864   -e cleaned.fasta > intron2.fasta
  alignment-cat -c 865-948   -e cleaned.fasta > exon3.fasta
  alignment-cat -c 949-1328  -e cleaned.fasta > intron3.fasta
  alignment-cat -c 1329-1393 -e cleaned.fasta > exon4.fasta

4. Put the pieces back together

  alignment-cat exon1.fasta intron1.fasta exon2.fasta intron2.fasta exon3.fasta intron3.fasta exon4.fasta > combined.fasta


5. Get the coding sequence

  alignment-cat exon{1,2,3,4}.fasta -c2-295 > coding.fasta
  sed -i 's/-/N/g' coding.fasta

5. Alignment with mafft and muscle

  mafft combined.fasta > combined-mafft.fasta
  muscle -in combined.fasta -out combined-muscle.fasta

6. Compute alignment diff

  alignments-diff combined-{mafft,muscle}.fasta > mafft.AU
  alignments-diff combined-{muscle,mafft}.fasta > muscle.AU
  alignment-draw combined-mafft.fasta  --AU mafft.AU  --scale=invert --show-ruler --color-scheme=Rainbow+fade[1,0]+contrast > mafft.html
  alignment-draw combined-muscle.fasta --AU muscle.AU --scale=invert --show-ruler --color-scheme=Rainbow+fade[1,0]+contrast > muscle.html

7. Compute dual diff

  alignments-diff combined-mafft.fasta combined-muscle.fasta  --merge --differences-file=mafft-muscle.AU --fill=unknown > mafft-muscle.fasta
  alignment-draw mafft-muscle.fasta --AU mafft-muscle.AU --scale=invert --color-scheme=Rainbow+fade[1,0]+contrast > mafft-muscle.html

8. run bali-phy

config.txt:
 align = exon1.fasta
 align = intron1.fasta
 align = exon2.fasta
 align = intron2.fasta
 align = exon3.fasta
 align = intron3.fasta
 align = exon4.fasta

 smodel = 1,3,5,7:gtr+Rates.free[n=4]
 imodel = 1,3,5,7:none

 smodel = 2,4,6:gtr+Rates.free[n=4]
 imodel = 2,4,6:rs07

9. run bali-phy with codon model on exons

This requires some work, since exon boundaries don't necessarily
fall between codons.  However, since we are already fixing the alignment,
we can concatenate the exons to get a sequence without stop codons.

 alignment-translate < coding.fasta

We can then use a codon model on the combined exons:

config2.txt
align = intron1.fasta
align = intron2.fasta
align = intron3.fasta
align = coding.fasta

smodel = 1,2,3:gtr+Rates.free[n=4]
imodel = 1,2,3:rs07

smodel = 4:gy94
#smodel = 4:fMutSel0
#smodel = 4:m3[gtr_sym,f3x4]
imodel = 4:none

