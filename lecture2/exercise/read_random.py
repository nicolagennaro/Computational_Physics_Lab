#!/usr/bin/env python3


import sys
import numpy as np

print("hello")

if( len(sys.argv) != 2 ):
    print("USAGE: " + sys.argv[0] + "  file_name")
    sys.exit(1)


with open(sys.argv[1], 'r') as f:

    v = []
    
    lines = f.readlines()
    for i in range(len(lines)):
        v.append(float(lines[i]))


print("read " + str(len(v))  + " lines")

v = np.array(v)

uniq = np.unique(v)

print("Unique vector: ")
print("Unique len: " + str(uniq.shape[0]))
print(uniq)
