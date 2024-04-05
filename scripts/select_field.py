#!/usr/bin/env python3

import sys
import argparse
import json

def extract_field(j,path):
    for elem in path:
        j = j[elem]
    return j

parser = argparse.ArgumentParser(description="Average piecewise-constant function")
parser.add_argument("filename",type=str,help="File containing JSON log")
parser.add_argument("path",type=str,help="Maximum percentags of Ns allowed")

args = parser.parse_args()

filename = args.filename

path = args.path.split('/')
for i in range(0,len(path)):
    if i < len(path)-1:
        path[i] = path[i]+'/'

with open(filename,'r',encoding='utf8') as file:
    next(file,None)
    for line in file:
        j = json.loads(line)
        print(json.dumps(extract_field(j,path)))


