% alignment-translate(1)
% Benjamin Redelings
% October 2026

# NAME

**alignment-translate** - Translate a DNA/RNA alignment into amino acids.

# SYNOPSIS

**alignment-translate** \[OPTIONS\] < _sequence-file_ > _output-file_

# DESCRIPTION

Read a DNA or RNA alignment from standard input and write its amino-acid translation to
standard output in FASTA format. Input may be FASTA or PHYLIP; the format and nucleotide
alphabet are detected automatically. Sequence names, comments, and order are preserved.
There is no positional filename argument: use shell redirection as shown in the synopsis.

By default, translate in frame 1 using the standard genetic code. The command can also
reverse or complement an alignment, with or without translation.

# OPTIONS

**-h**, **`--help`**
: Print usage and options, then exit.

**-g** _code_, **`--genetic-code`** _code_
: Select the genetic code. The default is `standard`. Use a name from GENETIC CODES or
  `code` followed by its number, for example `mt-vert` or `code2`. A bare number or a
  custom code filename is not accepted. Ignored when translation is disabled.

**-f** _frame_, **`--frame`** _frame_
: Select frame `1`, `2`, `3`, `-1`, `-2`, or `-3` (default: `1`). Positive frames skip
  zero, one, or two alignment columns before translating. Negative frames first take the
  reverse complement, then skip zero, one, or two columns from that end. Use, for example,
  `--frame=-2`. See TRANSFORMATION ORDER for interaction with other options.

**-r**, **`--reverse`**
: Reverse the order of alignment columns before translation. Does not disable translation.

**-c**, **`--complement`**
: Complement the nucleotides before translation. Does not disable translation. Combine with
  **`--reverse`** (or use `-rc`) to take the reverse complement.

**-t** _bool_, **`--translate`** _bool_
: Enable or disable translation (default: `yes`). Use `--translate=no` to output nucleotides
  after the explicitly requested reversal and/or complementation. This option requires a value.

# TRANSFORMATION ORDER

The **`--reverse`** and **`--complement`** operations are applied first. If **`--translate=no`**
is set, the resulting nucleotide alignment is written immediately: the frame causes neither
trimming nor an additional reverse complement, although its value must still be valid.

When translation is enabled, a negative frame applies an additional reverse complement.
The absolute frame then determines how many leading columns to skip. Thus `--frame=-2`
and `-rc --frame=2` are equivalent, while `-rc --frame=-2` cancels the two reverse
complements and translates the original alignment in frame 2.

# TRANSLATION DETAILS

Codons are consecutive groups of three alignment columns, including gaps. Gaps are not
removed before grouping, and each sequence uses the same column offset. Input should
therefore already be aligned in the desired codon frame. Shorter input sequences are
padded with gaps on the right to match the longest sequence before transformations.

Only complete groups of three columns are translated. Any remaining one or two columns
at the end are silently discarded. The command does not search for an open reading frame
or a start codon, and does not give initiation codons special treatment. Stop codons are
written as `*`; translation continues after them.

Within each group of three columns:

- Any gap (`-`) produces an amino-acid gap (`-`), even if the other positions are nucleotides
  or unknowns. Partial-codon gaps are not diagnosed or repaired.
- Otherwise, an unknown (`?`) produces `?`.
- Ambiguous nucleotides, including IUPAC ambiguity symbols and `N`, are translated by
  considering all compatible codons. If all give the same amino acid, that amino acid is
  written. The sets D/N, E/Q, and I/L are written as `B`, `Z`, and `J`, respectively.
  Other sets are written as `X`, which can include a possible stop. When `X` broadens a
  more restricted set of possible results, a warning is written to standard error.

# GENETIC CODES

The following names and numbers are supported. Each number is accepted with the prefix
`code` (for example, `code1` is equivalent to `standard`).

| Number | Name |
|-------:|:-----|
| 1 | `standard` |
| 2 | `mt-vert` |
| 3 | `mt-yeast` |
| 4 | `mt-protozoa` |
| 5 | `mt-invert` |
| 6 | `nuc-ciliate` |
| 9 | `mt-echinoderm` |
| 10 | `nuc-euplotid` |
| 11 | `bacteria` |
| 12 | `nuc-yeast-alt` |
| 13 | `mt-ascidian` |
| 14 | `mt-flatworm-alt` |
| 15 | `nuc-blepharisma` |
| 16 | `mt-chlorophycean` |
| 21 | `mt-trematode` |
| 22 | `mt-scenedesmus-obliquus` |
| 23 | `mt-thraustochytrium` |
| 24 | `mt-rhabdopleuridae` |
| 25 | `bacteria-sr1` |
| 26 | `nuc-pachysolen-tannophilus` |
| 27 | `nuc-karyorelict` |
| 28 | `nuc-condylostoma` |
| 29 | `nuc-mesodinium` |
| 30 | `nuc-peritrich` |
| 31 | `nuc-blastocrithidia` |
| 33 | `mt-cephalodiscidae` |

# EXAMPLES

Translate DNA or RNA to amino acids in reading frame 1 using the standard code:

```
alignment-translate < dna.fasta > aa.fasta
```

Translate using the vertebrate mitochondrial code:

```
alignment-translate --genetic-code=mt-vert < dna.fasta > aa.fasta
```

Write the reverse complement without translation:

```
alignment-translate -rc --translate=no < dna.fasta > reverse-complement.fasta
```

Translate in frame 2 of the reverse complement (these commands give the same result):

```
alignment-translate --frame=-2 < dna.fasta > aa2.fasta
alignment-translate -rc --frame=2 < dna.fasta > aa2.fasta
```

Translate a single sequence, retaining a stop codon (`MA*`):

```
printf '>example\nATGGCNTAA\n' | alignment-translate
```

# EXIT STATUS

Exit status is 0 on success (including **`--help`**) and 1 on error. Errors are reported on
standard error, including empty input, invalid sequence symbols, unsupported genetic codes,
and invalid frame values.

# SEE ALSO

**alignment-cat**(1), **alignment-info**(1)

# REPORTING BUGS
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
