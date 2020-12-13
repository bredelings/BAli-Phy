#!/usr/bin/env python3

import sys
import argparse
import json

parser = argparse.ArgumentParser(description="Average piecewise-constant function")
parser.add_argument("N",type=int,default=100,help="Number of points in reconstruction")
args = parser.parse_args()

N = args.N

def get_piecewise_fn_value(intervals,x):
    for rate,t1,t2 in intervals:
        if t1 <= x <= t2:
            return rate
    return None

def get_time_boundaries(intervals):
    (r0,t1_0,t2_0) = intervals[0]
    t1 = t1_0
    (rn,t1_n,t2_n) = intervals[-1]
    t2 = t2_n
    return (t1,t2)

def count_interval(N, total, intervals):
    t1,t2 = get_time_boundaries(intervals)
    for i in range(0,N):
        t = t1 + i*(t2-t1)/(N-1)
        total[i] += get_piecewise_fn_value(intervals,t)

with sys.stdin as file:
#       with open(filename,'r',encoding='utf8') as file:
    next(file,None)
    n = 0
    total = [0.0 for _ in range(0,N)]
    t1 = None
    t2 = None
    for line in file:
        n += 1
        intervals = json.loads(line)
        if t1 is None:
            t1,t2 = get_time_boundaries(intervals)
        count_interval(N, total, intervals)
    total2 = [ rate / n for rate in total]

    for i in range(0,N):
        t = t1 + i*(t2-t1)/(N-1)
        rate = total2[i]
        print(f"{t}\t{rate}")

