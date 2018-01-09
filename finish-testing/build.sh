#!/bin/bash

llc -filetype=obj -O2 test.ll && clang test.o -o test.out
