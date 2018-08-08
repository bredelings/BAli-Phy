% alignment-translate(1)
% Benjamin Redelings
% Feb 2018

# NAME

**alignment-translate** - Translate a DNA/RNA alignment into amino acids.

# SYNOPSIS

**alignment-translate** [OPTIONS] < sequence-file [OPTIONS]

# DESCRIPTION

Translate a DNA/RNA alignment into amino acids.

# ALLOWED OPTIONS:
**-h**, **--help**
: Produce help message

**-g** _arg_ (=standard), **--genetic-code** _arg_ (=standard)
: Specify alternate genetic code.

**-f** _arg_ (=1), **--frame** _arg_ (=1)
: Frame 1, 2, 3, -1, -2, or -3

**-r**, **--reverse**
: Just return the reverse

**-c**, **--complement**
: Just return the complement

**-t** _arg_ (=yes), **--translate** _arg_ (=yes)
: Translate the sequences


# EXAMPLES:
 



Translate DNA or RNA to amino acids in reading frame 1:

% alignment-translate < dna.fasta > aa.fasta



Give the reverse complement without translation:

% alignment-translate -rc --translate=no < dna.fasta > dna2.fasta



The following commands are identical:

% alignment-translate --frame=-2 < dna.fasta > aa2.fasta

% alignment-translate -rc --frame=2 < dna.fasta > aa2.fasta

# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

