; Find the first odd perfect number

(let [mod (lambda (n m) (- n (* (/ n m) m)))]
  (let [aliquot-sum (lambda (n)
                      (letrec [go (lambda (total i)
                                    (if0 i
                                      total
                                      (if0 (mod n i)
                                        (go (+ total i) (- i 1))
                                        (go total (- i 1)))))]
                        (go 0 (- n 1))))]
    (letrec [first-odd-perfect (lambda (n)
                                 (if0 (- n (aliquot-sum n))
                                   n
                                   (first-odd-perfect (+ n 2))))]
      (first-odd-perfect 1))))

