#!/bin/bash

iverilog -o test test.v
gcc -c -fpic vpi.c -I/usr/include/iverilog -I/usr/lib/ghc-8.0.1/include
ghc -shared -fPIC -o vpi.vpi -lvpi -dynamic -lHSrts-ghc8.0.1 VPITest.hs vpi.o
