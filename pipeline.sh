#!/bin/bash

stack run -- "$@" > gen.ll && llc-5.0 -filetype=asm -O2 gen.ll && clang gen.s -o gen.out && ./gen.out
