(require-extension
 syntax-case
 foof-loop
 check
 (srfi 11 19))
(require 'sound)
(import sound)
(let ((julian-day
       (date->julian-day
        (make-date 0 0 0 0 11 10 2008 0))))
  (let-values (((x-ecliptic y-ecliptic z-ecliptic
                            delta right-ascension declination)
                (allocentric-kepler mars
                                    earth-moon-barycenter
                                    julian-day)))
    (let ((date
           (date->string (julian-day->date julian-day))))
      (check (floor right-ascension) => (floor 3.71670244389746))
      (check (floor declination) => (floor -0.230086246671187))
      (check (floor delta) => (floor 2.49455512017685)))))
