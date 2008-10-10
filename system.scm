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
    (let ((system (keplerian-panner-fader
                   (pure-tone hertz (octave 4 c) size volume)
                   mars
                   earth-moon-barycenter
                   days
                   duration)))
      (loop ((for t (up-from 0 (to duration))))
            (let-values (((l r) (system t)))
              (endian-port-write-int1 out (+ (l t) (expt 2 (- size 1))))
              (endian-port-write-int1 out (+ (r t) (expt 2 (- size 1))))
              ))))
  (close-endian-port out))
