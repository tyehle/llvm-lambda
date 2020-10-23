(let [f (lambda (x)
          (lambda (y)
              (lambda (z) (- x y))))]
  (((f 11) 7) 5))