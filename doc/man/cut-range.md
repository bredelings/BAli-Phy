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
: the number of samples to skip

**--until** _arg_
: last sample to use

**--size** _arg_
: maximum number of samples to use

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

