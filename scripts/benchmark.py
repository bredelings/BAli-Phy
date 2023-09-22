#!/usr/bin/env python3

# Needs the /usr/bin/time program: apt install time

# Example: ./benchmark.py --dir ~/Work ~/Devel/bali-phy/old/bali-phy-3.6.0/bin/bali-phy ~/Devel/bali-phy/local/gcc-13/bin/bali-phy repo

import argparse
import re
import subprocess
import sys
import tempfile
import statistics
import os

def get_times(cmd):
    tmpfile = tempfile.NamedTemporaryFile()

    cmd = ['time' ,'-p','-o', tmpfile.name] + cmd
    result = subprocess.run(cmd, capture_output=True)

    if result.returncode != 0:
        print(result.stderr.decode(),file=sys.stderr)
        return None

    times = dict()
    tmpfile.seek(0)
    for line in tmpfile:
        m = re.match('([^ ]*) +([^ ]*)', line.decode())
        if m:
            times[m.group(1)] = float(m.group(2))

    return times

def benchmark(n, name, old_cmd, new_cmd):
    old_times = []
    new_times = []
    for i in range(n):
        print(f"testing {name}: iter {i+1}/n ",end='', flush=True)
        old_time = get_times(old_cmd)["user"];
        print(f'   old = {old_time}s',end='', flush=True)
        new_time = get_times(new_cmd)["user"];
        print(f'   new = {new_time}s   ratio = {new_time/old_time}')
        old_times.append(old_time)
        new_times.append(new_time)
    return (old_times, new_times)


parser = argparse.ArgumentParser(description="Extract the consensus sequence for a chromosome from a bam file",
                                 epilog= "Example: benchmark.py ERR2679006_dedup_reads.bam PVP01.fasta ERR2679006 --chromosome LT635626 --min-coverage 1 3 5 10 20")



parser.add_argument("old_file", help="Reference executable to test")
parser.add_argument("new_file", help="Executable to test")
parser.add_argument("repo", help="Path to repo")
parser.add_argument("--dir", default=".", help="Directory to run in")
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

os.chdir(args.dir)

# 1. build the exe

# 2. do timings on the exe

old_cmd = [args.old_file, '48-muscle.fasta', '--pre-burnin=0', '--iter=2']
new_cmd = [args.new_file, '48-muscle.fasta', '--pre-burnin=0', '--iter=2']

#old_cmd = ['ls','/dev/']
#new_cmd = ['ls','/usr/share/doc/']

print(f"old cmd = {old_cmd}", file=sys.stderr)
print(f"new cmd = {new_cmd}", file=sys.stderr)

old_times, new_times = benchmark(5, "sha", old_cmd, new_cmd)
med_old_time = statistics.median(old_times)
med_new_time = statistics.median(new_times)
print(f"old_time = {statistics.median(old_times)}, new_time = {statistics.median(new_times)}   ratio = {med_new_time/med_old_time}")




