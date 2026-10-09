% stats-cat(1)
% Benjamin Redelings
% October 2026

# NAME

**stats-cat** - Concatenate statistics tables or transform one MCON log.

# SYNOPSIS

**stats-cat** \[OPTIONS\] _file_ [_file_ ...]

# DESCRIPTION

By default, read one or more tab-separated statistics tables or MCON logs and
write one tab-separated table to standard output. The selected output column
names must agree across input files. Data rows are appended in file order, with
one output header. Use `-` as a filename to read standard input.

With **`--output json`**, transform one MCON log and write MCON: a JSON header
followed by one JSON object per sample. **`--unnest`** selects this mode when no
output format is specified. It replaces fields stored under MCON nesting keys
with their flat logical field names and updates the header's `nested` value.
Without **`--unnest`**, the nesting and its header value are retained.

In TSV output, **`--ignore`** and **`--select`** take column names or one-based
inclusive ranges such as `2:4`. In JSON output, they act on top-level JSON
field names after any unnesting. Each option may be repeated.

JSON output accepts exactly one input file. **`--skip`**, **`--subsample`**, and
**`--until`** apply only to TSV output; supplying them for JSON output is an
error. **`--unnest`** cannot be combined with **`--output tsv`**.

# OPTIONS

**-h**, **`--help`**
: Print usage information and exit.

**-s** _n_, **`--skip`** _n_
: Skip the first _n_ data rows of each input when writing TSV.

**-x** _n_, **`--subsample`** _n_
: Keep every _n_th data row after skipping when writing TSV. Default: 1.

**-u** _n_, **`--until`** _n_
: Consider at most the first _n_ data rows of each input when writing TSV.

**-I** _field_, **`--ignore`** _field_
: Exclude the named column or field. May be repeated.

**-S** _field_, **`--select`** _field_
: Include only the named columns or fields. May be repeated.

**-O** _format_, **`--output`** _format_
: Write `tsv` (the default) or `json` (MCON).

**`--unnest`**
: Flatten MCON nesting keys in JSON output. Implies **`--output json`**.

# EXAMPLES

Append two statistics tables:

```sh
stats-cat chain1.tsv chain2.tsv > combined.tsv
```

Convert one MCON log to TSV, keeping every tenth row:

```sh
stats-cat --subsample 10 chain.log.json > chain.tsv
```

Unnest one MCON log while retaining MCON output:

```sh
stats-cat --unnest chain.log.json > flat.log.json
```

# EXIT STATUS

Returns 0 on success and 1 on an error.

# SEE ALSO

**stats-select**(1), **stats-merge**(1), **mcon-tool**(1)

# REPORTING BUGS

BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
