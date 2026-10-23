% trees-to-SRQ(1)
% Benjamin Redelings
% October 2026

# NAME

**trees-to-SRQ** - Generate event histories and Scaled Regeneration Quantile plot coordinates.

# SYNOPSIS

**trees-to-SRQ** _PREDICATES_ _SAMPLED-TREES_ [_SAMPLED-TREES_ ...] \[OPTIONS\]

# DESCRIPTION

Examine when particular evolutionary relationships appear and disappear along a
sample of trees. A *predicate* is an event, such as the presence of a split dividing
A and B from C and D. Each retained tree gives a present/absent observation for that
event. **trees-to-SRQ** writes coordinates or binary values for plotting and further
analysis; it does not create an image file.

The default Scaled Regeneration Quantile (SRQ) curve shows the timing of returns to
an event. A return occurs when an absent observation is followed by a present one.
Several consecutive present observations count as one visit. Long stretches without
returns or changing return rates can indicate slow or uneven exploration of that
relationship during sampling.

The **sum** mode instead shows how the cumulative number of present observations
grows through the sample. **values** exports the original binary history. These
views complement support estimates: two histories can have the same fraction of
present trees but very different patterns of visits.

# INPUT AND PREDICATES

Supply Newick or NEXUS tree samples. Newick samples contain one tree per line;
**-** reads standard input. All samples and predicates must declare the same taxon
set, although their leaf orders may differ. Trees are interpreted as unrooted, with
an artificial degree-two root suppressed. Predicate branch lengths are ignored.

The predicate file contains blocks separated by blank lines. For example:

```text
A B | C D

A C | B D
```

These are two events. Multiple lines within a block form one event requiring all
of its relationships. A Newick tree supplies the conjunction of its splits; an
unresolved tree leaves some relationships unspecified. Duplicate predicates are
removed, retaining their first occurrence.

A partial split such as `A B | C D [ E ]` leaves E's placement unspecified. Declare
the same taxon universe on every line, listing excluded taxa in brackets. Taxa are
matched by name. **partitions-supported** can select predicates before this analysis.

# SAMPLE SELECTION

For each file, read at most **--until** input trees, discard **--skip** initial
trees, then retain the first survivor and every **--subsample**-th tree thereafter.
The default skip is **10%**. A percentage uses the shortest input run, after
**--until**, to calculate one common count rounded down; discard that count from
every run. Runs of 100 and 200 trees with **--skip 10%** therefore both lose ten.
Use **--skip 0** for already trimmed samples.

Retained files are concatenated in command order before constructing event
histories. A return can therefore occur at a join between files. To compare the
behavior of individual runs, generate their curves in separate invocations.
All time positions refer to this retained sequence, not original MCMC iterations.
**--max-points** affects output density only; it does not select sampled trees.

# MODES AND AXES

**SRQ**
: Write one curve per predicate. The vertical coordinate is the zero-based tree
  position of each return. Add a start boundary at zero and an end boundary at
  the number of retained trees. The horizontal coordinate numbers these points
  from zero, including the boundaries. By default, divide each axis by its final
  value so the curve runs from (0,0) to (1,1).

  Roughly evenly spaced returns tend to produce a roughly diagonal curve. A
  steep segment represents a long time interval between successive returns;
  a shallow segment represents closely spaced returns. The boundaries are not
  observed returns. With no returns, including an always-present or always-absent
  event, the output consists only of the two boundaries and is diagonal. Such a
  curve provides no evidence of frequent exploration. Inspect **values** or
  **sum** mode alongside it when there are few returns.

**sum**
: The horizontal coordinate is the number of retained trees examined, beginning
  at zero. The vertical coordinate is the cumulative number satisfying the
  predicate. With both axes unscaled, the average slope is the observed event
  frequency. Flat stretches are periods without the event.

  By default, both axes are divided by their final values. A nonzero sum curve
  then ends at (1,1), so its overall slope no longer estimates event frequency.
  A predicate never observed has a flat zero curve. Use **--no-scale-x** and
  **--no-scale-y** together for raw counts.

**values**
: Write a tab-separated header **P1**, **P2**, and so on, followed by one row per
  retained tree. Columns follow the distinct predicates in file order. Each value
  is 1 if the event is present and 0 otherwise. Axis scaling and point selection
  do not apply in this mode.

# CURVE OUTPUT AND POINT SELECTION

SRQ and sum output have two whitespace-separated columns, one point per line,
with a blank line between predicates. Curves follow predicate-file order and have
no headers. The coordinates can be read by plotting programs such as gnuplot.

**--max-points** sets an approximate density, not a strict maximum. Starting from
the last printed point, retain a point when either normalized coordinate advances
by more than 1 divided by the requested number. Always retain the endpoints.
This preserves large vertical changes as well as progress along the horizontal axis.
Selection uses normalized coordinates even when one or both output axes are unscaled.

# OPTIONS

**-h**, **--help**
: Print help and exit.

**--predicates** _FILE_
: Named form of the required positional predicate file.

**--files** _FILE_ ...
: Named form of the required positional sample files.

**-s**, **--skip** _COUNT-OR-PERCENT_
: Discard an initial count from every run, or derive the common count from a
  percentage of the shortest run. Default: **10%**.

**-u**, **--until** _N_
: Read at most the first _N_ input trees per file before skipping and subsampling.

**-x**, **--subsample** _N_
: Keep every _N_-th tree after skipping. Default: 1.

**--mode** _MODE_
: Select **SRQ**, **sum**, or **values**. Default: **SRQ**; names are case-sensitive.

**--max-points** _N_
: Approximate number of points per curve. Default: 1000. Use a positive value for
  curve output. Has no effect in **values** mode.

**--invert**
: Use absence of each predicate as the event. For a predicate with several
  relationships, this means at least one fails. This can make the rare absences
  of a frequently present event easier to inspect.

**--no-scale-x**
: Display the horizontal coordinate without dividing by its final value.

**--no-scale-y**
: Display the vertical coordinate without dividing by its final value.

# EXAMPLES

Create a predicate and a six-tree history:

```sh
printf 'A B | C D\n' > predicate.txt
cat > history.trees <<'TREES'
((A,C),B,D);
((A,C),B,D);
((A,B),C,D);
((A,B),C,D);
((A,C),B,D);
((A,B),C,D);
TREES
```

Export its binary history, **0, 0, 1, 1, 0, 1**, without discarding any trees:

```sh
trees-to-SRQ predicate.txt history.trees --skip 0 --mode values
```

The returns are at positions 2 and 5. Unscaled SRQ output has coordinates
(0,0), (1,2), (2,5), and (3,6), including the two boundaries:

```sh
trees-to-SRQ predicate.txt history.trees --skip 0 --no-scale-x --no-scale-y
```

Generate default normalized SRQ coordinates, raw cumulative counts, or the history
of the event's absence:

```sh
trees-to-SRQ predicate.txt history.trees --skip 0 > srq.txt
trees-to-SRQ predicate.txt history.trees --skip 0 --mode sum \
  --no-scale-x --no-scale-y > cumulative.txt
trees-to-SRQ predicate.txt history.trees --skip 0 --mode values --invert
```

The raw sum curve ends at (6,3), giving observed support 3/6 = 0.5.

# SEE ALSO

**partitions-supported**(1), **trees-bootstrap**(1), **trees-consensus**(1),
**trees-distances**(1)

# REPORTING BUGS

BAli-Phy online help: <https://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
