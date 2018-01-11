#!/bin/bash

stack build && stack exec llvm-lambda-exe > gen.ll && llc-5.0 -filetype=obj -O2 gen.ll && clang gen.o -o gen.out && ./gen.out
