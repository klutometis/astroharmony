(require-extension
 syntax-case
 endian-port
 foof-loop)
(require 'sound)
(import sound)
(let ((out (port->endian-port (open-output-file "test.raw")))
      (hertz 22050)
      (size 8)
      (seconds 5)
      (volume 0.5))
  (let ((t0 (pure-tone hertz (octave 4 c) size volume))
        (t1 (pure-tone hertz (octave 4 e) size volume))
        (t2 (pure-tone hertz (octave 4 gis) size volume))
        (t3 (pure-tone hertz (octave 5 b) size volume)))
    (let ((chord (mixer t0 t1 t2 t3))
          (p (period -1 1 (/ pi 2) hertz))
          (duration (* hertz seconds)))
      (let ((pan (periodic-pan
                  silence
                  (logarithmic-fader chord (/ duration 10) duration)
                  (dB -3)
                  (period -.1 -1 (/ pi 2) duration))))
        (loop ((for t (up-from 0 (to duration))))
              (let-values (((l r) (pan t)))
                (endian-port-write-int1 out (+ (l t) (expt 2 (- size 1))))
                (endian-port-write-int1 out (+ (r t) (expt 2 (- size 1)))))))))
  (close-endian-port out))
