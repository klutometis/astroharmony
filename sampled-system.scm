(require-extension
 syntax-case
 endian-port
 foof-loop
 (srfi 1 11 26))
(require 'sound)
(import sound)
(let ((out (port->endian-port (open-output-file "sampled.raw")))
      (hertz 2756)
      (size 8)
      (seconds 5)
      (days 365)
      (volume 0.5))
  (let ((sound (sampled-keplerian-panner-fader
                (pure-tone hertz (planet-tone jupiter) size volume)
                jupiter))
        (center (expt 2 (- size 1))))
    (loop ((for line (in-file "galileo.xyz" read-line)))
          (let*-values (((t x y z) (apply values (string-tokenize line)))
                        ((t x y z) (apply values (map string->number (list t x y z))))
                        ((left right) (sound t x y z)))
                       (endian-port-write-int1 out (+ (left t) center))
                       (endian-port-write-int1 out (+ (right t) center)))))
  (close-endian-port out))
