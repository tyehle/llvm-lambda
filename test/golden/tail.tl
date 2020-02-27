(let (sub (lambda (a b) (- a b)))
  (let (dec (lambda (n) (sub n 1)))
    (dec 5)))
