;(let (f (lambda (f g a b) (g f g (+ a b))))
;  (let (g (lambda (f g n) (if0 n 0 (f f g (- n 2) 1))))
;    (g f g 75000)))
0
