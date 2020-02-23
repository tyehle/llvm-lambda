(let (fact (lambda (n rec)
             (if0 n
               1
               (* n (rec
                      (- n 1)
                      rec)))))
  (fact 5 fact))
