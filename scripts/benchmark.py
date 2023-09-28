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
from git import Repo
import shutil
import logging
from pathlib import Path

def get_times(cmd, cwd):
    tmpfile = tempfile.NamedTemporaryFile()

    cmd = ['time' ,'-p','-o', tmpfile.name] + cmd
    result = subprocess.run(cmd, capture_output=True, cwd=cwd)

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

def benchmark(n, name, old_cmd, new_cmd, cwd):
    old_times = []
    new_times = []
    for i in range(n):
        print(f"testing {name}: iter {i+1}/n ",end='', flush=True)
        old_time = get_times(old_cmd, cwd)["user"];
        print(f'   old = {old_time}s',end='', flush=True)
        new_time = get_times(new_cmd, cwd)["user"];
        print(f'   new = {new_time}s   ratio = {new_time/old_time}')
        old_times.append(old_time)
        new_times.append(new_time)
    return (old_times, new_times)

def exec_show_result(cmd, **kwargs):
    showcmd = ' '.join([f"'{word}'" for word in cmd])

    subargs = dict()
    if "infile" in kwargs:
        infile = kwargs["infile"]
        subargs["stdin"] = open(infile,encoding='utf-8')
        showcmd += f" < '{infile}'"
    elif "stdin" in kwargs:
        subargs["stdin"] = kwargs["stdin"]

    if "outfile" in kwargs:
        outfile = kwargs["outfile"]
        subargs["stdout"] = open(outfile,'w+',encoding='utf-8')
        showcmd += f" > '{outfile}'"
    elif "stdout" in kwargs:
        subargs["stdout"] = kwargs["stdout"]
    else:
        subargs["stdout"] = subprocess.PIPE

    if "errfile" in kwargs:
        errfile = kwargs["errfile"]
        subargs["stderr"] = open(errfile,'w+',encoding='utf-8')
        showcmd += f" 2> '{errfile}'"
    elif "stderr" in kwargs:
        subargs["stderr"] = kwargs["stderr"]
    else:
        subargs["stderr"] = subprocess.PIPE

    if "cwd" in kwargs:
        subargs["cwd"] = kwargs["cwd"]

    logging.debug(showcmd)
    result = subprocess.run(cmd,**subargs)
    if result.returncode != 0:
        print(f"command: {showcmd}",file=sys.stderr)
        if "outfile" in kwargs and path.exists(kwargs["outfile"]):
            os.remove(kwargs["outfile"])
        if "errfile" in kwargs and path.exists(kwargs["errfile"]):
            os.remove(kwargs["errfile"])
        if "handler" in kwargs:
            handler = kwargs["handler"]
            handler(result.returncode)
    elif "verbose" in kwargs and kwargs["verbose"]:
        print(f"\n\t{showcmd}\n")
    return result

def exec_show(cmd, **kwargs):
    result = exec_show_result(cmd, **kwargs)

    out_message = None
    if "stdout" not in kwargs and "outfile" not in kwargs:
        out_message = result.stdout.decode('utf-8')

    err_message = None
    if "stderr" not in kwargs and "errfile" not in kwargs:
        err_message = result.stderr.decode('utf-8')

    # Always record error messages in the log file.
    if err_message:
        logging.info(f"  err: {err_message}")
    if out_message:
        logging.debug(f"  out: {out_message}")

    code = result.returncode
    if code != 0:
        logging.info(f" exit: {code}")

        logging.info(f" exit: {code}")
        if out_message:
            logging.info(f"  out: {out_message}")
            print(f"  out: {out_message}", file=sys.stdout)
        if err_message:
            logging.info(f"  err: {err_message}")

        if "stop_on_error" in kwargs and kwargs["stop_in_error"]:
            exit(code)
    return (out_message, result)

def configure(source_dir, build_dir, install_parent):

    if not os.path.isdir(build_dir):
        os.mkdir(build_dir)
        meson_out, meson_result = exec_show(['meson','setup', source_dir, build_dir, f'--prefix={install_parent}', '--reconfigure'])
        if meson_result.returncode != 0:
            sys.stderr.write(f"Removing directory {build_dir}!\n")
            logging.warning(f"Removing directory {build_dir}!")
            shutil.rmtree(build_dir)
            return False
        else:
            return True
    else:
        meson_out, meson_result = exec_show(['meson','setup', source_dir, build_dir, f'--prefix={install_parent}', '--reconfigure'])
        if meson_result.returncode != 0:
            return False
        else:
            return True;

def do_compile(build_dir):
    meson_out, meson_result = exec_show(['meson','install', '-C', build_dir])
    return meson_result.returncode == 0

def build(source_dir, build_dir, install_parent):
    if configure(source_dir, build_dir, install_parent):
        logging.info("configure: OK")
        if do_compile(build_dir):
            logging.info("compile: OK")
            return os.path.join(install_parent, "bin", "bali-phy")
        else:
            logging.info("compile: failed")
    else:
        logging.info("configure: failed")
    return None


parser = argparse.ArgumentParser(description="Extract the consensus sequence for a chromosome from a bam file",
                                 epilog= "Example: benchmark.py ERR2679006_dedup_reads.bam PVP01.fasta ERR2679006 --chromosome LT635626 --min-coverage 1 3 5 10 20")



parser.add_argument("--old", help="Reference executable to test")
parser.add_argument("--new", default=None,help="Executable to test")
parser.add_argument("repo", help="Path to repo")
parser.add_argument("--dir", default=".", help="Directory to run in")
parser.add_argument("--build", default="build",help="Directory to build in")
parser.add_argument("--install",default="local",help="Directory to install in")
parser.add_argument("--verbose",help="Be verbose",action='store_true')
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

if args.verbose:
    logging.basicConfig(level=0)
else:
    logging.basicConfig(level=15)

# 1. build the exe
exe = args.new

if exe is None:
    build_dir = Path(args.build).resolve()
    install_dir = Path(args.install).resolve()

    exe = build(args.repo, build_dir, install_dir)

# 2. do timings on the exe

old_cmd = [args.old, '48-muscle.fasta', '--pre-burnin=0', '--iter=6']
new_cmd = [exe, '48-muscle.fasta', '--pre-burnin=0', '--iter=6']

#old_cmd = ['ls','/dev/']
#new_cmd = ['ls','/usr/share/doc/']

print(f"old cmd = {old_cmd}", file=sys.stderr)
print(f"new cmd = {new_cmd}", file=sys.stderr)

run_dir = Path(args.dir)

old_times, new_times = benchmark(5, "sha", old_cmd, new_cmd, run_dir)
med_old_time = statistics.median(old_times)
med_new_time = statistics.median(new_times)
print(f"old_time = {statistics.median(old_times)}, new_time = {statistics.median(new_times)}   ratio = {med_new_time/med_old_time}")




# ok, so what do we have already done?
# right now we can get a ratio between a "new" executable and an "old" one.
# I guess the next thing that we need to do is to BUILD the executable from GIT sources.
