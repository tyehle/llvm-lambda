#!/usr/bin/env just --justfile

build:
    stack build
    clang -c gc.c

install-dev-deps:
    stack --install-ghc test --only-dependencies
    stack build hlint

@run:
    # to link without clang:
    # ld -dynamic-linker /lib64/ld-linux-x86-64.so.2 /usr/lib/x86_64-linux-gnu/crt1.o /usr/lib/x86_64-linux-gnu/crti.o -lc gen.o /usr/lib/x86_64-linux-gnu/crtn.o
    # for debugging the runtime: clang -c -flto -O3 -DDEBUG runtime.c
    stack run -- "$@" > gen.ll \
        && opt-9 -S -O3 gen.ll > gen-opt.ll \
        && llc-9 -filetype=obj -O3 gen-opt.ll \
        && clang -c -flto -O3 runtime.c \
        && clang -o gen.out -flto -O3 gen-opt.o runtime.o \
        && ./gen.out

test:
    stack test

lint:
    stack exec hlint -- app src test

check: test lint

push: check
    # don't push to master
    # ! git branch | grep '* master'
    git push

# Development

ghcid:
    stack exec -- ghcid --command='stack ghci --pedantic'

interactive-test:
    stack exec -- ghcid --run --clear --no-height-limit --command='stack ghci --test --main-is=llvm-lambda:test:llvm-lambda-test' --setup=':set args --hide-successes --color=always'

