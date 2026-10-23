% mcon-tool(1)
% Benjamin Redelings
% October 2026

# NAME

**mcon-tool** - Transform structured sample logs and convert them to tables.

# SYNOPSIS

**mcon-tool** \[OPTIONS\] [_FILE_]

# DESCRIPTION

Read a sample log, optionally transform its fields, and write MCON or tab-separated
values (TSV) to standard output. MCON consists of a JSON header followed by one JSON
object per sample. Unlike a rectangular table, it can hold arrays, objects, and
fields whose structure changes between samples.

Input may be MCON or a TSV table with a header row. TSV cells are parsed as JSON
values: numbers and booleans can be written directly, while strings require JSON
quotes. Omit the filename to read standard input; an explicit **-** is treated as
a literal filename.

Transformations run in a fixed order regardless of option order: drop fields,
unnest groups, atomize structured values, split by structure, and add record
numbers. Output defaults to MCON, preserving structure unless transformed.

# FIELDS AND TABLE OUTPUT

MCON grouping keys ending in a single slash contribute a prefix to field names;
keys ending in two slashes are transparent groups and contribute no prefix.
**--unnest** replaces this grouping with flat logical names. **--atomize** instead
expands structured field values into scalar components, with bracketed object keys
or one-based array indices.

For example, with nested grouping, a sample containing
`{"P1/":{"rates":[0.2,0.8]}}` becomes `{"P1/rates":[0.2,0.8]}` after unnesting.
Using both transformations gives fields `P1/rates[1]` and `P1/rates[2]`.

TSV conversion automatically expands structured values and retains fields with
stable structure across samples. Missing or structurally varying fields are
omitted and listed on standard error. A change in an array's length or an
object-valued parameter's keys removes that entire value: its remaining components
may have changed meaning. MCON groups contain separate fields, so a varying field
does not discard its siblings. Output column names have group prefixes shortened
where unambiguous.

Explicit **--atomize** expands values before this stability check, allowing
components to survive independently. **--split** can instead produce separate
tables for different structures, retaining fields that a combined table omits.

# OPTIONS

**-h**, **--help**
: Print help and exit.

**--filename** _FILE_
: Named form of the optional input filename.

**--unnest**
: Flatten MCON groups into logical field names. Structured field values remain
  intact unless **--atomize** is also supplied.

**--atomize**
: Expand structured values into scalar fields. Does not itself flatten MCON groups.

**--drop** _FIELD_ ...
: Remove fields by exact logical name, before unnesting or atomizing. For the
  example above, use `P1/rates` to remove the array. Put the input filename before
  this multiple-value option, or separate it from the field list with **--**.

**--split** _PREFIX_
: Group samples by structure after the requested transformations. Scalar values
  do not affect grouping, but scalar types, object keys, and array structure do;
  object key order is ignored. Write groups to _PREFIX_.1.json, _PREFIX_.2.json,
  etc., or use the .tsv extension with TSV output. Larger groups come first;
  equal-sized groups follow the order their structures first appeared.
  Samples retain their original order within each group.
  The complete transformed log is also written to standard output.
  Existing output files are overwritten.

**-T**, **--add-time-field** _NAME_
: Add a field containing consecutive record numbers starting at zero. Numbering
  restarts in each split file; stdout uses the complete log's numbering.
  These are record positions, not original MCMC iteration numbers.
  An existing field with the same logical name is an error.

**-O**, **--output** _FORMAT_
: Write **MCON** (the default) or **TSV**. The lowercase spellings **mcon** and
  **tsv** are also accepted.

# EXAMPLES

Convert a structured log to a table of stable fields:

    mcon-tool run-1/C1.log.json --output TSV > samples.tsv

Remove a structured parameter and flatten the remaining group names:

    mcon-tool samples.json --unnest --drop P1/rates > reduced.json

Write one TSV file per structure, discarding the combined stdout table:

    mcon-tool samples.json --split part --output TSV > /dev/null

# EXIT STATUS

Returns 0 on success (including help) and 1 on an error.

# SEE ALSO

**statreport**(1), **stats-cat**(1), **stats-select**(1)

# REPORTING BUGS

BAli-Phy online help: <https://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
