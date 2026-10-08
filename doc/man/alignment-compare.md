% alignment-compare(1)
% Benjamin Redelings
% October 2026

# NAME

**alignment-compare** - Compare two alignment distributions.

# SYNOPSIS

**alignment-compare** [OPTIONS] _sample-file1_ _sample-file2_ _target-alignment_

# DESCRIPTION

Compare two alignment distributions.

All three filenames are required positional arguments. The third specifies the target alignment
to annotate; `-` reads the target from standard input. Sample filenames do not interpret `-`
as standard input.

# ALLOWED OPTIONS:
**-h**, **`--help`**
: produce help message

**`--alphabet`** _arg_
: Specify the alphabet: DNA, RNA, Amino-Acids, Amino-Acids+stop, Triplets, Codons, or Codons+stop.

**`--seed`** _arg_
: random seed

**`--max-alignments`** _arg_ (=1000)
: Maximum retained alignments per sample. Samples are thinned to this limit; `-1` means unlimited.

**`--verbose`**
: Output more log messages on stderr.


# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
