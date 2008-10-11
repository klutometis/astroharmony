(require-extension
 syntax-case
 endian-port
 foof-loop)
(require 'sound)
(import sound)
(let ((out (port->endian-port (open-output-file "test.raw")))
      (hertz 11025)
      (size 8)
      (seconds 1)
      (days 365)
      (volume 0.5))
  (let ((duration (* hertz seconds)))
    (let ((planets
           (map (lambda (planet)
                  (keplerian-panner-fader
                   (pure-tone hertz (planet-tone planet) size volume)
                   planet
                   earth-moon-barycenter
                   days
                   duration))
                (list mercury
                      venus
                      mars
                      jupiter
                      saturn
                      uranus
                      neptune
                      pluto))))
      (let-values (((mercury
                     venus
                     mars
                     jupiter
                     saturn
                     uranus
                     neptune
                     pluto)
                    (apply values planets)))
        (loop ((for t (up-from 0 (to duration))))
              (let-values (((mercury-l mercury-r) (mercury t))
                           ((venus-l venus-r) (venus t))
                           ((mars-l mars-r) (mars t))
                           ((jupiter-l jupiter-r) (jupiter t))
                           ((saturn-l saturn-r) (saturn t))
                           ((uranus-l uranus-r) (uranus t))
                           ((neptune-l neptune-r) (neptune t))
                           ((pluto-l pluto-r) (pluto t)))
                (if (zero? (modulo t 100))
                    (debug (- duration t)))
                (endian-port-write-int1 out (+ ((mixer
                                                 mercury-l
                                                 venus-l
                                                 mars-l
                                                 jupiter-l
                                                 saturn-l
                                                 neptune-l
                                                 pluto-l) t)
                                               (expt 2 (- size 1))))
                (endian-port-write-int1 out (+ ((mixer
                                                 mercury-r
                                                 venus-r
                                                 mars-r
                                                 jupiter-r
                                                 saturn-r
                                                 neptune-r
                                                 pluto-r) t)
                                               (expt 2 (- size 1)))))))))
  (close-endian-port out))
