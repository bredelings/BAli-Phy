#!/usr/bin/env python3

# Needs the /usr/bin/time program: apt install time

import argparse
import re
import subprocess
import sys
import tempfile

def get_times(exe):
    tmpfile = tempfile.NamedTemporaryFile()
    cmd = [exe, '48-muscle.fasta', '--seed=0', '--pre-burnin=0', '--iter=25']
    cmd = ['ls']

    cmd = ['time' ,'-p','-o', tmpfile.name] + cmd
    result = subprocess.run(cmd, capture_output=True)
    tmpfile.seek(0)

    times = dict()
    for line in tmpfile:
        m = re.match('([^ ]*) +([^ ]*)', line.decode())
        if m:
            times[m.group(1)] = float(m.group(2))

    return times

parser = argparse.ArgumentParser(description="Extract the consensus sequence for a chromosome from a bam file",
                                 epilog= "Example: benchmark.py ERR2679006_dedup_reads.bam PVP01.fasta ERR2679006 --chromosome LT635626 --min-coverage 1 3 5 10 20")



parser.add_argument("file", help="Executable to test")
#parser.add_argument("reference", help="Reference file used to map the bam file (.fasta)")
#parser.add_argument("--chromosome", help="The chromosome from which to extract the consensus sequence")
#parser.add_argument("--min-quality", default=25, type=int, help="Quality score required for a base in a read to count (default is 25)")
#parser.add_argument("--min-coverage", default=[2], type=int,nargs='+',help="Number of high quality score reads required to make a call (default is 2)")
#parser.add_argument("--max-coverage",type=int, help="Ignore reference locations with coverage higher than this")
#parser.add_argument("--prefix", help="Prefix chromosome names with this")
#parser.add_argument("--min-mapping-quality", default=1, type=int,nargs='+',help="Skip reads with a lower mapping quality (default is 1)")
#parser.add_argument("--max-soft-clipped", type=int, default=0, help="Skip reads with a higher number of soft-clipped bases (default is 0)")
#parser.add_argument("output", help="Output prefix")
args = parser.parse_args()

# 1. build the exe

# 2. do timings on the exe
exe = args.file

times = get_times(exe)

print(f"times = {times}")
