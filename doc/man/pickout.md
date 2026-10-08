% pickout(1)
% Benjamin Redelings
% October 2026

# NAME

**pickout** - Extract a table from lines containing named values.

# SYNOPSIS

**pickout** \[OPTIONS\] _field1_ [_field2_ ...] < _data-file_

# DESCRIPTION

Read standard input and write a tab-separated table to standard output. Supply one or more
field names as positional arguments; output columns follow their argument order. The first
output line contains these names unless **`--no-header`** is given.

A matching input line must contain every requested field in the form `field = value`, with
exactly the shown spaces around `=`. Lines missing any requested field are skipped. Matching
is case-sensitive and uses the first occurrence of each `field = ` substring; it does not
require the field name to start at a word boundary.

Normally a value ends at the next space outside parentheses. Double-quoted values may contain
spaces and retain their surrounding quotes. Escaped quotes are not supported. The last
requested field can instead extend to the end of its line or across multiple lines using the
options below. Here, "last" means last on the command line, not last in the input line.

# OPTIONS

**-h**, **`--help`**
: Print usage and options, then exit.

**-n**, **`--no-header`**
: Suppress the line of field names.

**`--large`**
: Take the last requested value through the end of its input line, including spaces and any
  later fields. Takes precedence over **`--multi-line`** if both are supplied.

**`--multi-line`**
: Take the last requested value through the end of its input line and append subsequent
  lines until an empty line or end of input. The empty separator line is consumed but not
  included. Embedded newlines are preserved, so an output record may span multiple lines.
  All requested fields must still occur on the initial matching line.

# EXAMPLES

Extract two columns, in the requested order:

```sh
printf 'iteration = 1 score = -12\niteration = 2 score = -10\n' | pickout score iteration
```

The output is tab-separated:

```text
score	iteration
-12	1
-10	2
```

Extract the remainder of a line after `pi = ` without a header:

```sh
pickout --no-header --large pi < run.log
```

# REPORTING BUGS

BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
