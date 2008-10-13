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
      (seconds 1)
      (days 365)
      (volume 0.5)
      (planets (list
                mercury
                venus
                earth-moon-barycenter
                mars
                jupiter
                saturn
                uranus
                neptune
                pluto)))
  (let ((duration (* (+ (length planets) 1) hertz seconds))
        (center (expt 2 (- size 1)))
        (t0 (current-julian-day))
        (n-planets (length planets)))
    (let* ((heliocentric
            (interval/silence
             (keplerian-panner-faders
              sol
              t0
              days
              hertz
              size
              seconds
              volume
              planets)
             0
             (* seconds hertz)
             (lambda (t) (values silence silence))))
           (sounds
            (cons heliocentric
                  (map
                   (lambda (si ti planet)
                     (interval/silence
                      (keplerian-panner-faders
                       planet
                       (+ t0 ti)
                       days
                       hertz
                       size
                       seconds
                       volume
                       (delete planet planets))
                      si
                      (+ si (* seconds hertz))
                      (lambda (t) (values silence silence))))
                   (iota n-planets
                         (* seconds hertz)
                         (* seconds hertz))
                   (iota n-planets
                         days
                         days)
                   planets)))
           (system (apply stereo-adder sounds)))
      (loop ((for t (up-from 0 (to duration))))
            (if (zero? (modulo t 1000))
                (debug (- duration t)))
            (let-values (((left right)
                          (system t)))
              (endian-port-write-int1 out (+ (left t) center))
              (endian-port-write-int1 out (+ (right t) center))))))
  (close-endian-port out))
