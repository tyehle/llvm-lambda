FROM fpco/stack-build:lts-9

RUN apt-get update && apt-get install -y llvm-5.0-dev clang-5.0

ADD stack.yaml .
ADD package.yaml .
RUN stack --no-terminal --install-ghc test --bench --only-dependencies

# RUN stack --no-terminal build

# CMD stack --no-terminal test --bench --no-run-benchmarks --haddock --no-haddock-deps