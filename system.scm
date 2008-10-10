(require-extension
 syntax-case
 endian-port
 foof-loop)
(require 'sound)
(import sound)
(let ((out (port->endian-port (open-output-file "test.raw")))
      (hertz 22050)
      (size 8)
      (seconds 1)
      (days 365)
      (volume 0.5))
  (let ((duration (* hertz seconds)))
    (let ((mercury (keplerian-panner-fader
                    (pure-tone hertz (octave 4 c) size volume)
                    mercury
                    earth-moon-barycenter
                    days
                    duration))
          (venus (keplerian-panner-fader
                  (pure-tone hertz (octave 4 e) size volume)
                  venus
                  earth-moon-barycenter
                  days
                  duration))
          (mars (keplerian-panner-fader
                 (pure-tone hertz (octave 4 g) size volume)
                 mars
                 earth-moon-barycenter
                 days
                 duration)))
      (loop ((for t (up-from 0 (to duration))))
            (let-values (((mercury-l mercury-r) (mercury t))
                         ((venus-l venus-r) (venus t))
                         ((mars-l mars-r) (mars t)))
              (endian-port-write-int1 out (+ ((mixer
                                               mercury-l
                                               venus-l
                                               mars-l) t) (expt 2 (- size 1))))
              (endian-port-write-int1 out (+ ((mixer
                                               mercury-r
                                               venus-r
                                               mars-r) t) (expt 2 (- size 1))))
              ))))
  (close-endian-port out))
