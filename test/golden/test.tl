(let (mk-add (lambda (n) (lambda (x) (+ x n))))
  (let (a1 (mk-add 1))
    (let (a2 (lambda (x) (a1 (a1 x))))
      (+ (a1 3) (a2 5)))))
