#!/bin/bash
./build/compiler $1 > $2
./assmule $2 | grep -v "\-\-\-" | grep -v -i "steve gregory" | ./scripts/drop 4
