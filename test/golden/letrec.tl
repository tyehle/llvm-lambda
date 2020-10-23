(letrec [f (lambda (x) (if0 x 0 (f (- x 1))))]
  (f 2))