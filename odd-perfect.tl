; Find the first odd perfect number

(let (mod (lambda (n m) (- n (* (/ n m) m))))
  (let (aliquot-sum (lambda (n)
                      (let (go (lambda (rec total i)
                                 (if0 i
                                   total
                                   (if0 (mod n i)
                                     (rec rec (+ total i) (- i 1))
                                     (rec rec total (- i 1))))))
                        (go go 0 (- n 1)))))
    (let (first-odd-perfect (lambda (rec n)
                              (if0 (- n (aliquot-sum n))
                                n
                                (rec rec (+ n 2)))))
      (first-odd-perfect first-odd-perfect 1))))

