#!/usr/bin/python
from __future__ import print_function

def get_alignments(infile):
    alignment = None
    last_sample = None
    with open(infile, 'r') as infilestream:
        for line in infilestream:
            m = re.match("Sample (\d+)\s+Alignment:\s+(.*)$",line)
            if m is not None:
                current_sample = int(m.group(1))
                a = m.group(2)
                # when starting a new group, yield the last alignment and start a new one
                if last_sample != current_sample:
                    if alignment is not None:
                        yield alignment
                    alignment = []
                    last_sample = current_sample
                alignment.append(a)
# Since we only yield an alignment when we start a new one,
# the last one must be handled separately
    yield alignment

def print_alignment(alignment):
    for line in alignment:
        print(line)

if __name__ == '__main__':
    import codecs
    import re
    import sys
    import os

    infile = sys.argv[1]

    num = 0
    for alignment in get_alignments(infile):
        num += 1
        print_alignment(alignment)
        print("\n\n")
    sys.stderr.write("Read {} alignments.\n".format(num))
