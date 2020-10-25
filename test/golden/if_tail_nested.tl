(letrec (f (lambda (n)
             (if0 n
               (f (+ n 1))
               (if0 (- n 1)
                 (f (- n 1))
                 n))))
  (f 2))