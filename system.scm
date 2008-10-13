(require-extension
 syntax-case
 endian-port
 foof-loop
 (srfi 1 26))
(require 'sound)
(import sound)
(let ((out (port->endian-port (open-output-file "test.raw")))
      (hertz 2756)
      (size 8)
      (seconds 5)
      (days 365)
      (volume 0.5)
      (targets (list
                mercury
                venus
                earth-moon-barycenter
                mars
                jupiter
                saturn
                uranus
                neptune
                pluto)))
  (let* ((sources (cons sol targets))
         (n-sources (length sources)))
    (let ((partial-duration (* hertz seconds))
          (duration (* n-sources hertz seconds))
          (center (expt 2 (- size 1)))
          (julian-zero (current-julian-day)))
      (loop ((with sources sources (cdr sources))
             (with current-julian julian-zero (+ current-julian days))
             (with ti 0 (+ ti partial-duration))
             (until (null? sources)))
            (let* ((source (car sources))
                   (sound
                    (keplerian-panner-faders
                     source
                     current-julian
                     days
                     hertz
                     size
                     seconds
                     volume
                     (delete source targets))))
              (loop ((for t (up-from ti (to (+ ti partial-duration)))))
                    (if (zero? (modulo t 1000))
                        (debug (- duration t)))
                    (let-values (((left right) (sound t)))
                      (endian-port-write-int1 out (+ (left t) center))
                      (endian-port-write-int1 out (+ (right t) center))))))))
  (close-endian-port out))
