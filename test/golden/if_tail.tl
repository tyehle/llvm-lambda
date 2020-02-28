(let (f (lambda () 5))
  (let (main (lambda ()
               (if0 0
                 (f)
                 (f))))
    (main)))
