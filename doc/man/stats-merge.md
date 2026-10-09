% stats-merge(1)
% Benjamin Redelings
% October 2026

# NAME

**stats-merge** - Join corresponding lines from statistics files.

# SYNOPSIS

**stats-merge** _file_ [_file_ ...]

# DESCRIPTION

Read the named files in parallel and write each set of corresponding lines to
standard output, separated by tabs. The first lines, normally the column
headers, are joined in the same way as data lines. All files must contain the
same number of lines.

The program does not compare shared column values such as an `iter` field;
files must already have matching row order. At least one filename is required.

# OPTIONS

**-h**, **`--help`**
: Print usage information and exit.

# EXAMPLE

Join columns from two logs with matching rows:

```sh
stats-merge chain.parameters.tsv chain.statistics.tsv > chain.combined.tsv
```

# EXIT STATUS

Returns 0 on success and 1 on an error, including files with different numbers
of lines.

# SEE ALSO

**stats-cat**(1), **stats-select**(1)

# REPORTING BUGS

BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
