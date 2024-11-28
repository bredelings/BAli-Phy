% alignment-draw(1)
% Benjamin Redelings
% Feb 2018

# NAME

**alignment-draw** - Draw an alignment to HTML, optionally coloring residues by AU.

# SYNOPSIS

**alignment-draw** _alignment_ [_AU file_] [OPTIONS]

# DESCRIPTION

Draw an alignment to HTML, optionally coloring residues by AU.

# GENERAL OPTIONS:
**-h**, **--help**
: produce help message

**--file** _arg_
: file with sequences and initial alignment

**--legend**
: Print a legend showing how color-scheme indicates uncertainty.

**--show-ruler**
: Print a ruler to show column numbers

**--column-colors**
: Color-code column ticks by column certainty

**--AU** _arg_
: file with alignment uncertainties

**--show-gaps** _arg_ (=yes)
: Show gaps

**--show-letters** _arg_ (=yes)
: Show letters

**--show-names** _arg_ (=yes)
: Show names

**--gaps-different** _arg_ (=yes)
: Color gaps in grey.

**--width** _arg_
: The number of columns per line

**--start** _arg_
: The first column to plot

**--end** _arg_
: The last column to plot

**--format** _arg_ (=HTML)
: produce a plot in this format

**--min** _arg_
: Minimum value of scale function

**--max** _arg_
: Maximum value of scale function

**--color-scheme** _arg_
: Include a length of how certainties map to colors

**--scale** _arg_ (=LOD)
: scale for the uncertainties


# EXAMPLES:
 Rainbow+contrast+fade
AA+contrast+fade+fade+fade+fade

# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

