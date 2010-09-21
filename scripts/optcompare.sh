#!/bin/bash
./build/compiler $1 > $2
echo $3 | ./assmule $4 $2 | tail -n 3
echo "optimised:"
./build/opt-compiler $1 > $2
echo $3 | ./assmule $4 $2 | tail -n 3
