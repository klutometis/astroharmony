(require-extension
 syntax-case
 foof-loop
 (srfi 11 19))
(require 'sound)
(import sound)
(let ((j (current-julian-day)))
  (loop ((for k (up-from 0 (to 365))))
        (let*-values (((x-ecliptic y-ecliptic z-ecliptic
                                   delta right-ascension declination)
                       (allocentric-kepler mars
                                           earth-moon-barycenter
                                           (+ j k)))
                      ((x-equatorial y-equatorial z-equatorial)
                       (equatorials x-ecliptic y-ecliptic z-ecliptic)))
          (let ((date
                 (date->string (julian-day->date (+ (current-julian-day) k)))))
            (debug
             date
             x-equatorial
             y-equatorial
             z-equatorial
;;;              x-ecliptic
;;;              y-ecliptic
;;;              z-ecliptic
;;;              (degrees->minutes (radians->degrees right-ascension))
;;;              (radians->degrees declination)
             delta
             )))))
