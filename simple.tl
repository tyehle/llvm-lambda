(letrec (f (lambda (n)
             (if0 n
               0
               (f (- n 1)))))
  (f 175000))
