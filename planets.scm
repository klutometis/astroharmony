;;; Simplified models for 1800 AD -- 2050 AD; also includes a star and
;;; dwarf planet.
(define sol
  (make-planet (make-element 1e-12 0.0)
               (make-element 0.0 0.0)
               (make-element 0.0 0.0)
               (make-element 0.0 0.0)
               (make-element 0.0 0.0)
               (make-element 0.0 0.0)))

(define mercury
  (make-planet (make-element 0.38709927 0.00000037)
               (make-element 0.20563593 0.00001906)
               (make-element 7.00497902 -0.00594749)
               (make-element 252.25032350 149472.67411175)
               (make-element 77.45779628 0.16047689)
               (make-element 48.33076593 -0.12534081)))

(define venus
  (make-planet (make-element 0.72333566 0.00000390)
               (make-element 0.00677672 -0.00004107)
               (make-element 3.39467605 -0.00078890)
               (make-element 181.97909950 58517.81538729)
               (make-element 131.60246718 0.00268329)
               (make-element 76.67984255 -0.27769418)))

(define earth-moon-barycenter
  (make-planet (make-element 1.00000261 0.00000562)
               (make-element 0.01671123 -0.00004392)
               (make-element -0.00001531 -0.01294668)
               (make-element 100.46457166 35999.37244981)
               (make-element 102.93768193 0.32327364)
               (make-element 0.0 0.0)))

(define mars
  (make-planet (make-element 1.52371034 0.00001847)
               (make-element 0.09339410 0.00007882)
               (make-element 1.84969142 -0.00813131)
               (make-element -4.55343205 19140.30268499)
               (make-element -23.94362959 0.44441088)
               (make-element 49.55953891 -0.29257343)))

(define jupiter
  (make-planet (make-element 5.20288700 -0.00011607)
               (make-element 0.04838624 -0.00013253)
               (make-element 1.30439695 -0.00183714)
               (make-element 34.39644051 3034.74612775)
               (make-element 14.72847983 0.21252668)
               (make-element 100.47390909 0.20469106)))

(define uranus
  (make-planet (make-element 19.18916464 -0.00196176)
               (make-element 0.04725744 -0.00004397)
               (make-element 0.77263783 -0.00242939)
               (make-element 313.23810451 428.48202785)
               (make-element 170.95427630 0.40805281)
               (make-element 74.01692503 0.04240589)))

(define neptune
  (make-planet (make-element 30.06992276 0.00026291)
               (make-element 0.00859048 0.00005105)
               (make-element 1.77004347 0.00035372)
               (make-element -55.12002969 218.45945325)
               (make-element 44.96476227 -0.32241464)
               (make-element 131.78422574 -0.00508664)))

(define pluto
  (make-planet (make-element 39.48211675 -0.00031596)
               (make-element 0.24882730 0.00005170)
               (make-element 17.14001206 0.00004818)
               (make-element 238.92903833 145.20780515)
               (make-element 224.06891629 -0.04062942)
               (make-element 110.30393684 -0.01183482)))
