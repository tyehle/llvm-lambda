#!/bin/bash

stack run -- "$@" > gen.ll && llc-5.0 -filetype=asm -O2 gen.ll && clang -c gc.c && clang gen.s gc.o -o gen.out && ./gen.out

# to link without clang:
# ld -dynamic-linker /lib64/ld-linux-x86-64.so.2 /usr/lib/x86_64-linux-gnu/crt1.o /usr/lib/x86_64-linux-gnu/crti.o -lc gen.o /usr/lib/x86_64-linux-gnu/crtn.o
