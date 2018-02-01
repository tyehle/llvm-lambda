#!/bin/bash

# llc -filetype=obj -O2 test.ll && clang test.o -o test.out

llc test.ll
as test.s -o test.o
ld -o test.out -dynamic-linker=/lib64/ld-linux-x86-64.so.2 /usr/lib/x86_64-linux-gnu/crt1.o /usr/lib/x86_64-linux-gnu/crti.o test.o -lc /usr/lib/x86_64-linux-gnu/crtn.o
