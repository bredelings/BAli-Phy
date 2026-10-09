% stats-select(1)
% Benjamin Redelings
% October 2026

# NAME

**stats-select** - Select columns and rows from a statistics table.

# SYNOPSIS

**stats-select** \[OPTIONS\] [_column_ ...] < _data-file_

# DESCRIPTION

Read a tab-separated statistics table or MCON log from standard input and write
a tab-separated table to standard output. Positional column names choose the
columns to keep, in their original order. With **`--remove`**, they instead
identify columns to omit. With no column names, all columns are retained.

Column arguments may also be one-based inclusive numeric ranges, such as `2:4`,
`:3`, or `5:`. Row conditions refer to the input table, so a column used for
filtering need not appear in the output. Each **`--select`** condition requires
an exact `key=value` match; repeated conditions must all match.

# OPTIONS

**-h**, **`--help`**
: Print usage information and exit.

**`--no-header`**
: Omit the output line of column names.

**-s** _key=value_, **`--select`** _key=value_
: Keep only rows with the given value in the named input column. May be repeated.

**-r**, **`--remove`**
: Remove the listed columns instead of keeping them.

# EXAMPLES

Keep the `likelihood` column for rows where `chain=1`:

```sh
stats-select --select chain=1 likelihood < samples.tsv > likelihood.tsv
```

Remove the second and third columns:

```sh
stats-select --remove 2:3 < samples.tsv > remaining.tsv
```

# EXIT STATUS

Returns 0 on success and 1 on an error.

# SEE ALSO

**stats-cat**(1), **stats-merge**(1)

# REPORTING BUGS

BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
