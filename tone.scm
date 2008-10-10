(require-extension
 syntax-case
 endian-port
 foof-loop)
(require 'sound)
(import sound)
(let ((out (port->endian-port (open-output-file "test.raw")))
      (hertz 22050)
      (size 8)
      (seconds 10)
      (volume 0.5))
  (let ((duration (* hertz seconds)))
    (let ((mercury (pure-tone hertz (planet-tone mercury) size volume))
          (venus (pure-tone hertz (planet-tone venus) size volume))
          (earth-moon-barycenter
           (pure-tone hertz (planet-tone earth-moon-barycenter) size volume))
          (mars (pure-tone hertz (planet-tone mars) size volume))
          (jupiter (pure-tone hertz (planet-tone jupiter) size volume))
          (saturn (pure-tone hertz (planet-tone saturn) size volume))
          (uranus (pure-tone hertz (planet-tone uranus) size volume))
          (neptune (pure-tone hertz (planet-tone neptune) size volume))
          (pluto (pure-tone hertz (planet-tone pluto) size volume)))
      (let ((chord (logarithmic-fader
                    (mixer mercury
                           venus
                           earth-moon-barycenter
                           mars
                           jupiter
                           saturn
                           uranus
                           neptune
                           pluto)
                    (/ duration 10)
                    duration)))
        (loop ((for t (up-from 0 (to duration))))
              (endian-port-write-int1 out (+ (chord t) (expt 2 (- size 1))))))))
  (close-endian-port out))
