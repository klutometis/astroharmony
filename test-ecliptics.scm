(require-extension
 syntax-case
 foof-loop
 check
 (srfi 11 19))
(require 'sound)
(import sound)

(define tolerance 1)

(define (within-tolerance? x y)
  (= (round (/ x tolerance))
     (round (/ y tolerance))))

(let ((julian-day
       (date->julian-day
        (make-date 0 0 0 0 11 10 2008 0))))
  (let-values (((mercury-x-ecliptic
                 mercury-y-ecliptic
                 mercury-z-ecliptic
                 mercury-delta
                 mercury-right-ascension
                 mercury-declination)
                (allocentric-kepler mercury
                                    earth-moon-barycenter
                                    julian-day))
               ((venus-x-ecliptic
                 venus-y-ecliptic
                 venus-z-ecliptic
                 venus-delta
                 venus-right-ascension
                 venus-declination)
                (allocentric-kepler venus
                                    earth-moon-barycenter
                                    julian-day))
               ((mars-x-ecliptic
                 mars-y-ecliptic
                 mars-z-ecliptic
                 mars-delta
                 mars-right-ascension
                 mars-declination)
                (allocentric-kepler mars
                                    earth-moon-barycenter
                                    julian-day))
               ((jupiter-x-ecliptic
                 jupiter-y-ecliptic
                 jupiter-z-ecliptic
                 jupiter-delta
                 jupiter-right-ascension
                 jupiter-declination)
                (allocentric-kepler jupiter
                                    earth-moon-barycenter
                                    julian-day))
               ((saturn-x-ecliptic
                 saturn-y-ecliptic
                 saturn-z-ecliptic
                 saturn-delta
                 saturn-right-ascension
                 saturn-declination)
                (allocentric-kepler saturn
                                    earth-moon-barycenter
                                    julian-day))
               ((uranus-x-ecliptic
                 uranus-y-ecliptic
                 uranus-z-ecliptic
                 uranus-delta
                 uranus-right-ascension
                 uranus-declination)
                (allocentric-kepler uranus
                                    earth-moon-barycenter
                                    julian-day))
               ((neptune-x-ecliptic
                 neptune-y-ecliptic
                 neptune-z-ecliptic
                 neptune-delta
                 neptune-right-ascension
                 neptune-declination)
                (allocentric-kepler neptune
                                    earth-moon-barycenter
                                    julian-day))
               ((pluto-x-ecliptic
                 pluto-y-ecliptic
                 pluto-z-ecliptic
                 pluto-delta
                 pluto-right-ascension
                 pluto-declination)
                (allocentric-kepler pluto
                                    earth-moon-barycenter
                                    julian-day))
               )
      (check mercury-right-ascension (=> within-tolerance?) (hms->radians 12 34 02.34))
      (check mercury-declination (=> within-tolerance?) (dms->radians -04 23 10.0))
      (check mercury-delta (=> within-tolerance?) .701727137853550)
      (check venus-right-ascension (=> within-tolerance?) (hms->radians 15 11 15.42))
      (check venus-declination (=> within-tolerance?) (dms->radians -18 30 34.5))
      (check venus-delta (=> within-tolerance?) 1.33267915020603)
      (check mars-right-ascension (=> within-tolerance?) (hms->radians 14 09 03.90))
      (check mars-declination (=> within-tolerance?) (dms->radians -12 56 03.6))
      (check mars-delta (=> within-tolerance?) 2.49362543620074)
      (check jupiter-right-ascension (=> within-tolerance?) (hms->radians 19 01 18.53))
      (check jupiter-declination (=> within-tolerance?) (dms->radians -23 01 36.6))
      (check jupiter-delta (=> within-tolerance?) 5.11315971751320)
      (check saturn-right-ascension (=> within-tolerance?) (hms->radians 11 12 17.14))
      (check saturn-declination (=> within-tolerance?) (dms->radians +07 00 18.2))
      (check saturn-delta (=> within-tolerance?) 10.1785846635222)
      (check uranus-right-ascension (=> within-tolerance?) (hms->radians 23 22 36.70))
      (check uranus-declination (=> within-tolerance?) (dms->radians -04 53 58.1))
      (check uranus-delta (=> within-tolerance?) 19.2151037492343)
      (check neptune-right-ascension (=> within-tolerance?) (hms->radians 21 35 53.36))
      (check neptune-declination (=> within-tolerance?) (dms->radians -14 40 43.3))
      (check neptune-delta (=> within-tolerance?) 29.4727573455549)
      (check pluto-right-ascension (=> within-tolerance?) (hms->radians 17 54 19.18))
      (check pluto-declination (=> within-tolerance?) (dms->radians -17 29 14.1))
      (check pluto-delta (=> within-tolerance?) 31.8489673821327)
      ))
