(define (newton f df guess tolerance)
  (let iter ((guess guess))
    (let* ((next-guess (- guess (/ (f guess) (df guess))))
           (delta (abs (- guess next-guess))))
      (print guess)
      (if (< delta tolerance)
          guess
          (iter next-guess)))))
