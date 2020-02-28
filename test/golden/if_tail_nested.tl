(let (f (lambda (f n)
          (if0 n
            (f f (+ n 1))
            (if0 (- n 1)
              (f f (- n 1))
              n))))
  (f f 2))