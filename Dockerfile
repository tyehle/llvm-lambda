FROM fpco/stack-build:lts-9

RUN apt-get update && apt-get install -y llvm-5.0-dev clang-5.0
RUN ln -s /usr/bin/clang-5.0 /usr/bin/clang

WORKDIR /root/llvm-lambda

ADD stack.yaml .
ADD package.yaml .
RUN stack --no-terminal --install-ghc test --bench --only-dependencies
