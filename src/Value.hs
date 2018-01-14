module Value where

import LLVM.AST

data Value = Value Operand
           | Closure String Operand


{-

(((lambda (x)
    (lambda (y)
      (+ x y)))
  2)
 3)

-----

int main() {
  void* clos = f0(3, null);
  printf("%d\n", result);
  int result = f1(2, clos);
}

void* f0(int x, void* env) {
  void* outEnv = malloc(sizeof int);
  outEnv[0] = x;
  return outEnv;
}

int f1(int y, void* env) {
  int x = env[0];
  return x + y;
}

-}
