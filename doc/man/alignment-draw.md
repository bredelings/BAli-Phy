% alignment-draw(1)
% Benjamin Redelings
% Feb 2018

# NAME

**alignment-draw** - Draw an alignment and interactively inspect character properties.

# SYNOPSIS

**alignment-draw** _alignment_ [_AU file_] [OPTIONS]

# DESCRIPTION

Draw an alignment to HTML, optionally coloring cells by alignment uncertainty
(AU). With **--properties**, the output is a self-contained interactive page:
it embeds the scientific data, styles, and JavaScript and does not require a
network connection.

The property selector supports linear, log10, and empirical-rank color scales.
Linear and log10 scales can use the full range, a robust 2--98% range, or custom
bounds. The legend and cell colors update together, and pointing at or focusing
a non-gap cell shows its raw posterior mean and retained-sample count.

When **--AU** and **--properties** are supplied together, the viewer can fade
the property color toward white according to posterior alignment certainty.
Property values use ungapped sequence-character coordinates; AU values use
alignment-grid coordinates. The AU file must therefore have exactly the same
number of model-character columns as the displayed alignment.

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

**--properties** _arg_
: JSON property summary produced by **calc-properties**

**--alphabet** _arg_
: alphabet used to tokenize model characters; required with **--properties**

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

Create a property summary and an interactive DNA alignment:

```
calc-properties run-1/C1.properties1.json run-2/C1.properties1.json \
  --skip=1000 > P1.character-properties.json

alignment-draw P1.initial.fasta --alphabet DNA \
  --properties P1.character-properties.json > P1.initial.html
```

Add posterior alignment uncertainty to the same interactive page:

```
alignment-draw P1.initial.fasta --alphabet DNA \
  --properties P1.character-properties.json --AU P1.initial-AU.prob \
  > P1.initial-AU.html
```

# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
