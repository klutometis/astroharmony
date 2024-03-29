(require-extension
 syntax-case
 endian-port
 foof-loop
 (srfi 1 26))
(require 'sound)
(import sound)
(let ((out (port->endian-port (open-output-file "test-emb-source.raw")))
      (hertz 2756)
      (size 8)
      (seconds 30)
      (days (* 3 365))
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
  (let ((duration (* hertz seconds))
        (center (expt 2 (- size 1))))
    (let ((panner-fader (keplerian-panner-faders
                         earth-moon-barycenter
                         (current-julian-day)
                         days
                         hertz
                         size
                         seconds
                         volume
                         (delete earth-moon-barycenter
                                 planets)))
          (source-sound (pure-tone hertz (planet-tone earth-moon-barycenter) size volume)))
      (loop ((for t (up-from 0 (to duration))))
            (if (zero? (modulo t 1000))
                (debug (- duration t)))
            (let-values (((left right)
                          (panner-fader t)))
              (endian-port-write-int1 out (+ ((mixer left (amplifier source-sound 0.1)) t) center))
              (endian-port-write-int1 out (+ ((mixer right (amplifier source-sound 0.1)) t) center))))))
  (close-endian-port out))
