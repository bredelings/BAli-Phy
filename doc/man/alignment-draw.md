% alignment-draw(1)
% Benjamin Redelings
% October 2026

# NAME

**alignment-draw** - Display an alignment in HTML with optional uncertainty and character properties.

# SYNOPSIS

**alignment-draw** \[OPTIONS\] _ALIGNMENT_ \[_ANNOTATIONS_\]

# DESCRIPTION

Draw an alignment as an HTML page for viewing in a web browser. Color letters by nucleotide or
amino-acid identity, or use alignment annotations to show which residue placements are uncertain
or differ from another alignment. A residue is a sequence unit, such as a nucleotide or amino acid.

With **--properties**, inspect posterior character properties alongside the alignment: color cells
by a selected property, examine estimates and their uncertainty, and locate columns with high
values. Property summaries describe individual sequence characters, whereas alignment annotations
describe their placement in this particular alignment.

The HTML is written to standard output. Redirect it to a file and open that file in a browser.
The page includes its data, styles, and scripts and can be used without a network connection.
Diagnostics go to standard error.

# INPUT

ALIGNMENT is a required positional filename containing one alignment. Use `-` to read standard
input.

ANNOTATIONS is an optional positional filename containing an alignment annotation table, such as
one produced by **alignment-gild**, **alignment-compare**, or **alignments-diff**. The format is also
called AU, for alignment uncertainty. The header lists sequence names, separated by spaces. Each
subsequent row describes one alignment column, with one value per sequence in header order and a
final, unlabelled value for the whole column. A `-` denotes a missing value.

Sequence names must match the displayed alignment, though their order may differ. The table must
have exactly one row per model-character column: for a codon alphabet, one codon is one column.
Probability annotations express certainty, with larger values indicating better-supported
placements. Comparison categories from **alignments-diff** instead need **--scale identity** and
a **diff[N]** color scheme.

**--properties** reads a JSON summary produced by **character-properties summarize** and requires
**--alphabet**. Property values are associated with a sequence name and ungapped character
position. Displayed column numbers locate those characters in the chosen alignment; they do not
identify a column shared by all sampled alignments.

# COLOR SCHEMES

**--color-scheme** takes a base scheme followed by optional modifiers joined with `+`, for example
`DNA+contrast+fade`. The default is `plain` without annotations and `Rainbow+fade` with annotations.

Base schemes are:

- `plain`: black letters on white.
- `DNA`, `RNA`, `AA`: colors based on nucleotide or amino-acid identity.
- `bw`: certainty shown in grayscale.
- `Rainbow`, `RedBlue`, `BlueRed`: certainty shown by a progression of colors.
- `diff[N]`: white for matching residues and N colors for difference categories from
  **alignments-diff**. Use the same N as that program's **--classes** value.

Modifiers apply in the order written: `switch` exchanges foreground and background colors;
`contrast` selects black or white letters for contrast with the background; `fade` fades colors
toward white as certainty decreases. Modifiers can be repeated.

**--scale** transforms annotation values before applying the color scheme. `identity` passes
values through unchanged, including comparison categories. The other choices transform a
probability p, clip it to the chosen bounds, and rescale that interval to 0–1:

- `invert`: 1 − p; default bounds 0 and 1.
- `square`: p²; default bounds 0 and 1.
- `cube`: p³; default bounds 0 and 1.
- `LOD`: log10(p / (1 − p)); default bounds −0.5 and 2. This is the default scale.

**--min** and **--max** override bounds in these transformed coordinates. They do not affect
`identity`. These command-line scales apply to alignment annotations; property color scales
are selected separately in the browser.

# PROPERTY VIEWER

Choose a property to color the alignment by its posterior estimates. Point at or focus a non-gap
cell to see its mean, standard deviation, and median. The legend follows the active scale and
palette. The page also shows the number of retained samples.

Property scales include linear, log10, and empirical rank. Linear and log10 scales offer the full
range, a robust 2–98% range, or custom bounds. Scale and palette selections are retained separately
for each property; Reset restores the initial settings.

The default diverging palette runs from blue through light gray to red. For positive-selection
probabilities, it initially maps 0, 0.5, and 1 to those colors on a linear scale. For other
properties, gray marks the median. Viridis and a blue-to-red sequential palette are also available.

The ranked panel selects one representative observed character per nonempty alignment column.
Order generic properties by column, mean, or standard deviation. Positive-selection properties
can be filtered at probabilities 0.5, 0.95, and 0.99, with the corresponding **dNdS** summary when
available. Selecting a row highlights its alignment column and opens the character's tooltip.

**Original colors** restores alphabet colors while keeping property tooltips available. Codons
occupy one logical cell with three nucleotide-colored parts; their report rows use the selected
genetic code for amino-acid translation. When annotations and properties are both supplied, the
viewer can fade property colors toward white according to alignment certainty.

# OPTIONS

**-h**, **--help**
: Print help and exit.

**--alphabet** _ALPHABET_
: Alphabet used to interpret model characters, for example `DNA`, `Amino-Acids`, or
  `Codons(DNA,standard)`. Required with **--properties**.

**--properties** _FILE_
: JSON posterior character-property summary from **character-properties summarize**.

**--color-scheme** _SCHEME_
: Base color scheme and optional modifiers. Default: `Rainbow+fade` with annotations,
  `plain` otherwise. See COLOR SCHEMES.

**--scale** _SCALE_
: Annotation transform: `identity`, `invert`, `square`, `cube`, or `LOD`. Default: `LOD`.

**--min** _VALUE_, **--max** _VALUE_
: Lower and upper bounds in transformed annotation coordinates. Defaults depend on **--scale**.

**--legend**
: Show how the color scheme represents alignment certainty.

**--show-ruler**
: Show column numbers above the alignment.

**--column-colors**
: Color ruler ticks using the whole-column annotation values.

**--show-gaps** _BOOL_
: Show gap symbols. Default: `yes`; use `no` to hide them.

**--show-letters** _BOOL_
: Show sequence letters. Default: `yes`; use `no` to show colors without letters.

**--show-names** _BOOL_
: Show sequence names. Default: `yes`.

**--gaps-different** _BOOL_
: Use gray for gaps in schemes that distinguish them. Default: `yes`.

**--width** _N_
: Model-character columns per line. Default: twice the alignment length, displaying the
  alignment on one line.

**--start** _N_, **--end** _N_
: First and last columns to display, inclusive and zero-based. Default: the entire alignment.
  Displayed ruler labels are one-based.

# EXAMPLES

Create an HTML alignment with nucleotide colors:

    alignment-draw alignment.fasta --color-scheme DNA+contrast --show-ruler > alignment.html

Color an alignment using certainty annotations from **alignment-gild**:

    alignment-draw alignment.fasta alignment-AU.prob --legend --show-ruler > uncertainty.html

Create a property summary and view it alongside a DNA alignment:

    character-properties summarize run1/C1.P1.site-property-samples.jsonl \
      run2/C1.P1.site-property-samples.jsonl --skip 1000 > properties.json
    alignment-draw alignment.fasta --alphabet DNA \
      --properties properties.json > properties.html

Combine property colors with alignment certainty:

    alignment-draw alignment.fasta alignment-AU.prob --alphabet DNA \
      --properties properties.json > properties-AU.html

# SEE ALSO

**alignment-gild**(1), **alignment-compare**(1), **alignments-diff**(1), **character-properties**(1)

# REPORTING BUGS

BAli-Phy online help: <https://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
