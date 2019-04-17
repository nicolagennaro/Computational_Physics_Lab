#!/usr/bin/env python

import numpy as np
import sys

if( len(sys.argv) < 2 ):
    print( "USAGE: {}  file".format( sys.argv[0] ) )


d = []
    
with open( sys.argv[1], "r" ) as f:
    lines = f.readlines()
    for _ in range(len(lines)):
        d.append( float(lines[_]) )


d = np.array(d)

print( "d.shape = {}".format( d.shape ) )

sh = int(input("input shape: "))

d = d.reshape(( 100, sh))

print( d.shape )

m = np.mean(d, axis = 1)
s = np.std(d, axis = 1)

print("mean: {}".format( m ) )
print("std : {}".format( s ) )



with open( "clean.txt", "w" ) as f:
    for _ in range( m.shape[0] ):
        f.write( "{}\t{}\t{}\n".format( _, m[_], s[_]) )
