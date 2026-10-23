% cut-range(1)
% Benjamin Redelings
% Feb 2018

# NAME

**cut-range** - Select lines from multiple input files based on lines containing `key = value`.

# SYNOPSIS

**cut-range** filename1.fastas [filename2.fastas ...] [OPTIONS]

# DESCRIPTION

Select lines from multiple input files based on lines containing `key = value`.

# ALL OPTIONS:
**-h**, **--help**
: produce help message

**--key** _arg_ (=iterations)
: cut based on values of _key_=value

**--skip** _arg_
: Minimum key value to retain, inclusive (an iteration number by default).

**--until** _arg_
: Maximum key value to retain, inclusive.

**--size** _arg_
: Key-value span to retain. The inclusive upper bound is _skip_ + _size_, using zero
  when **--skip** is omitted. This limits the range of key values, not the number of
  samples. Cannot be combined with **--until**.

**--verbose**
: Output more log messages on stderr.


# EXAMPLES:
 
Select alignments after the first 100 iterations from two different runs:
```
% cut-range --skip=100 run-1/C1.P1.fastas run-2/C1.P1.fastas > P1.fastas
```


# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
