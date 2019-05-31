#!/bin/bash

for sigma in 0.5 0.6 0.707106781187 0.8 0.9 1.; do
    cat > input << EOF
1000
$sigma
0.
5.
EOF

./metropolis_gaussian.f90 < input >> dati

done
