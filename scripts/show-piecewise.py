#!/usr/bin/env python3

import sys
import argparse
import json
import statistics

# Example:
# ./show-piecewise LiStephens2003Hotspot-7/C1.log.json parameters/rho --output=median > a
# gnuplot
# > plot 'a' with lines

def get_piecewise_fn_value(intervals,x):
    lastRate = None
    for rate,t1,t2 in intervals:
        if t1 <= x <= t2:
            return rate
        else:
            lastRate = rate
    return lastRate

def get_domain_boundaries(intervals):
    (r0,t1_0,t2_0) = intervals[0]
    t1 = t1_0
    (rn,t1_n,t2_n) = intervals[-1]
    t2 = t2_n
    return (t1,t2)

def do_interval(N, values, intervals):
    t1,t2 = get_domain_boundaries(intervals)
    for i in range(0,N):
        t = t1 + i*(t2-t1)/(N-1)
        values[i].append( get_piecewise_fn_value(intervals,t) )

def make_grid(N, intervals):
    t1,t2 = get_domain_boundaries(intervals)
    grid = []
    for i in range(0,N):
        t = t1 + i*(t2-t1)/(N-1)
        grid.append( get_piecewise_fn_value(intervals, t) )
    return grid
        
def extract_field(j,path):
    j2 = j.copy()
    for elem in path:
        j2 = j2[elem]
    return j2

def set_field(j,path,value):
    path2 = path.copy()
    last = path2.pop()
    for elem in path2:
        j = j[elem]
    j[last] = value

def get_path(path_string):
    if not path_string:
        return []

    path = path_string.split('/')
    for i in range(0,len(path)):
        if i < len(path)-1:
            path[i] = path[i]+'/'
    return path

parser = argparse.ArgumentParser(description="Average piecewise-constant function")
parser.add_argument("filename",help="MCON file to analyze")
parser.add_argument("field",help="Field to analyze")
parser.add_argument("--skip",type=int,default=0,help="lines to skip")
parser.add_argument("-N",type=int,default=100,help="Number of points in reconstruction")
parser.add_argument("--output",default="median",help="Output: mean, median, grid")
args = parser.parse_args()

field = get_path(args.field)
N = args.N
skip = args.skip
output = args.output

infile=None
if args.filename == '-':
    infile=sys.stdin
else:
    infile=open(args.filename,"r", encoding='utf8')

# Skip file header    
header = next(infile,None)
if args.output=="grid":
    print(header,end='')

n = 0
values = [[] for _ in range(0,N)]
t1 = None
t2 = None
for line in infile:
    n += 1;
    j = json.loads(line)
    intervals = extract_field(j, field)
    if args.output == "grid":
        intervals = make_grid(N, intervals)
        set_field(j, field, intervals)
        print(json.dumps(j))
    elif n > skip:
        if t1 is None:
            t1,t2 = get_domain_boundaries(intervals)
        do_interval(N, values, intervals)

if args.output == "mean" or args.output == "median":        
    for i in range(0,N):
        t = t1 + i*(t2-t1)/(N-1)
        if  args.output == "mean":
            rate = statistics.mean(values[i])
        elif  args.output == "median":
            rate = statistics.median(values[i])
        print(f"{t}\t{rate}")

