(define-record-type :element
  (make-element j2000 rate)
  element?
  (j2000 element-j2000)
  (rate element-rate))

(define-record-type :planet
  (make-planet semi-major-axis
               eccentricity
               inclination
               mean-longitude
               longitude-perihelion
               longitude-ascending-node
               orbit
               octave)
  planet?
  (semi-major-axis planet-semi-major-axis)
  (eccentricity planet-eccentricity)
  (inclination planet-inclination)
  (mean-longitude planet-mean-longitude)
  (longitude-perihelion planet-longitude-perihelion)
  (longitude-ascending-node planet-longitude-ascending-node)
  (orbit planet-orbit)
  (octave planet-octave))

(define (orbit->hertz days)
  (let ((seconds (* days 60 60)))
    (/ 1 seconds)))

(define (planet-tone planet)
  (* (orbit->hertz (planet-orbit planet))
     (expt 2 (planet-octave planet))))

;;; Simplified models for 1800 AD -- 2050 AD; also includes a star and
;;; dwarf planet.
(define sol
  (make-planet (make-element 1e-12 0.0)
               (make-element 0.0 0.0)
               (make-element 0.0 0.0)
               (make-element 0.0 0.0)
               (make-element 0.0 0.0)
               (make-element 0.0 0.0)
               0.0
               0.0))

(define mercury
  (make-planet (make-element 0.38709927 0.00000037)
               (make-element 0.20563593 0.00001906)
               (make-element 7.00497902 -0.00594749)
               (make-element 252.25032350 149472.67411175)
               (make-element 77.45779628 0.16047689)
               (make-element 48.33076593 -0.12534081)
               87.9691
               28))

(define venus
  (make-planet (make-element 0.72333566 0.00000390)
               (make-element 0.00677672 -0.00004107)
               (make-element 3.39467605 -0.00078890)
               (make-element 181.97909950 58517.81538729)
               (make-element 131.60246718 0.00268329)
               (make-element 76.67984255 -0.27769418)
               224.70069
               29))

(define earth-moon-barycenter
  (make-planet (make-element 1.00000261 0.00000562)
               (make-element 0.01671123 -0.00004392)
               (make-element -2.67209908480332e-07 -0.000225962193202099)
               (make-element 1.75343755707279 628.307577900922)
               (make-element 1.79660147404917 0.00564218940290684)
               (make-element 0.0 0.0)
               365.256366
               30))

(define mars
  (make-planet (make-element 1.52371034 0.00001847)
               (make-element 0.09339410 0.00007882)
               (make-element 0.0322832054248893 -0.00014191813200034)
               (make-element -0.0794723815383351 334.061301681387)
               (make-element -0.41789517122344 0.00775643308768542)
               (make-element 0.864977129749742 -0.00510636965735315)
               668.5991
               31))

(define jupiter
  (make-planet (make-element 5.20288700 -0.00011607)
               (make-element 0.04838624 -0.00013253)
               (make-element 1.30439695 -0.00183714)
               (make-element 34.39644051 3034.74612775)
               (make-element 14.72847983 0.21252668)
               (make-element 100.47390909 0.20469106)
               4331.572
               33))

(define saturn
  (make-planet (make-element 9.53667594 -0.00125060)
               (make-element 0.05386179 -0.00050991)
               (make-element 2.48599187 0.00193609)
               (make-element 49.95424423 1222.49362201)
               (make-element 92.59887831 -0.41897216)
               (make-element 113.66242448 -0.28867794)
               10832.327
               34))

(define uranus
  (make-planet (make-element 19.18916464 -0.00196176)
               (make-element 0.04725744 -0.00004397)
               (make-element 0.77263783 -0.00242939)
               (make-element 313.23810451 428.48202785)
               (make-element 170.95427630 0.40805281)
               (make-element 74.01692503 0.04240589)
               30799.095
               35))

(define neptune
  (make-planet (make-element 30.06992276 0.00026291)
               (make-element 0.00859048 0.00005105)
               (make-element 1.77004347 0.00035372)
               (make-element -55.12002969 218.45945325)
               (make-element 44.96476227 -0.32241464)
               (make-element 131.78422574 -0.00508664)
               60190
               36))

(define pluto
  (make-planet (make-element 39.48211675 -0.00031596)
               (make-element 0.24882730 0.00005170)
               (make-element 17.14001206 0.00004818)
               (make-element 238.92903833 145.20780515)
               (make-element 224.06891629 -0.04062942)
               (make-element 110.30393684 -0.01183482)
               90613.3055
               37))
