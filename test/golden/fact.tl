(letrec (fact (lambda (n)
                (if0 n
                  1
                  (* n (fact (- n 1))))))
  (fact 5))
