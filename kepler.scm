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
               b
               c
               s
               f)
  planet?
  (semi-major-axis planet-semi-major-axis)
  (eccentricity planet-eccentricity)
  (inclination planet-inclination)
  (mean-longitude planet-mean-longitude)
  (longitude-perihelion planet-longitude-perihelion)
  (longitude-ascending-node planet-longitude-ascending-node)
  (b planet-b)
  (c planet-c)
  (s planet-s)
  (f planet-f))

(define (radians->degrees . radians)
  (apply values (map (cut * <> (/ 180.0 pi)) radians)))

(define (degrees->radians . degrees)
  (apply values (map (cut * <> (/ pi 180.0)) degrees)))

(define (alternate-ecliptics true-anomaly
                             heliocentric-radius
                             longitude-ascending-node
                             longitude-perihelion
                             inclination)
  (let ((true-anomaly (degrees->radians true-anomaly))
        (longitude-ascending-node (degrees->radians longitude-ascending-node))
        (longitude-perihelion (degrees->radians longitude-perihelion))
        (inclination (degrees->radians inclination)))
    (let ((internal-term (+ true-anomaly
                            longitude-perihelion
                            (- longitude-ascending-node))))
      (let ((x (* heliocentric-radius (- (* (cos longitude-ascending-node)
                                            (cos internal-term))
                                         (* (sin longitude-ascending-node)
                                            (sin internal-term)
                                            (cos inclination)))))
            (y (* heliocentric-radius (+ (* (sin longitude-ascending-node)
                                            (cos internal-term))
                                         (* (cos longitude-ascending-node)
                                            (sin internal-term)
                                            (cos inclination)))))
            (z (* heliocentric-radius
                  (sin internal-term)
                  (sin inclination))))
        (values x y z)))))

(define (ecliptics argument-perihelion
                   longitude-ascending-node
                   inclination
                   x-heliocentric
                   y-heliocentric
                   z-heliocentric
                   source-x-ecliptic
                   source-y-ecliptic
                   source-z-ecliptic)
  (let-values (((argument-perihelion
                 longitude-ascending-node
                 inclination)
                (degrees->radians
                 argument-perihelion
                 longitude-ascending-node
                 inclination)))
    (let ((x (+ (* (- (* (cos argument-perihelion)
                         (cos longitude-ascending-node))
                      (* (sin argument-perihelion)
                         (sin longitude-ascending-node)
                         (cos inclination)))
                   x-heliocentric)
                (* (- (- (* (sin argument-perihelion)
                            (cos longitude-ascending-node)))
                      (* (cos argument-perihelion)
                         (sin longitude-ascending-node)
                         (cos inclination)))
                   y-heliocentric)))
          (y (+ (* (+ (* (cos argument-perihelion)
                         (sin longitude-ascending-node))
                      (* (sin argument-perihelion)
                         (cos longitude-ascending-node)
                         (cos inclination)))
                   x-heliocentric)
                (* (+ (- (* (sin argument-perihelion)
                            (sin longitude-ascending-node)))
                      (* (cos argument-perihelion)
                         (cos longitude-ascending-node)
                         (cos inclination)))
                   y-heliocentric)))
          (z (+ (* (sin argument-perihelion)
                   (sin inclination)
                   x-heliocentric)
                (* (cos argument-perihelion)
                   (sin inclination)
                   y-heliocentric))))
      (values (- x source-x-ecliptic)
              (- y source-y-ecliptic)
              (- z source-z-ecliptic)))))

(define (equatorials x-ecliptic
                     y-ecliptic
                     z-ecliptic
                     obliquity)
  (let ((obliquity (degrees->radians obliquity)))
    (let ((x-equatorial x-ecliptic)
          (y-equatorial (+ (* (cos obliquity)
                              y-ecliptic)
                           (- (* (sin obliquity)
                                 z-ecliptic))))
          (z-equatorial (+ (* (sin obliquity)
                              y-ecliptic)
                           (* (cos obliquity)
                              z-ecliptic))))
      (values x-equatorial y-equatorial z-equatorial))))

(define (compute-element element T)
  (+ (element-j2000 element) (* (element-rate element) T)))

(define (compute-elements T . elements)
  (apply values (map (cut compute-element <> T) elements)))

(define (eccentric-anomaly mean-anomaly eccentricity* eccentricity tolerance)
  (let iter ((eccentric-anomaly
              (+ mean-anomaly
                 (* eccentricity*
                    (sin (degrees->radians mean-anomaly))))))
    (let* ((delta-mean-anomaly
            (- mean-anomaly
               (- eccentric-anomaly
                  (* eccentricity*
                     (sin (degrees->radians eccentric-anomaly))))))
           (delta-eccentric-anomaly
            (/ delta-mean-anomaly
               (- 1 (* eccentricity
                       (cos (degrees->radians eccentric-anomaly)))))))
      (if (< (abs delta-eccentric-anomaly) tolerance)
          eccentric-anomaly
          (iter (+ eccentric-anomaly delta-eccentric-anomaly))))))

(define (true-anomaly eccentricity eccentric-anomaly)
  (modulo
   (radians->degrees
    (* 2 (atan (* (sqrt (/ (+ 1 eccentricity) (- 1 eccentricity)))
                  (tan (* 0.5 (degrees->radians eccentric-anomaly)))))))
   360))

(define (heliocentric-radius . heliocentrics)
  (sqrt
   (apply
    +
    (map (lambda (heliocentric) (expt heliocentric 2)) heliocentrics))))

(define (heliocentric-radius-alt semi-major-axis eccentricity true-anomaly)
  (/ (* semi-major-axis (- 1 (expt eccentricity 2)))
     (+ 1 (* eccentricity (cos (degrees->radians true-anomaly))))))

(define (heliocentrics semi-major-axis eccentric-anomaly eccentricity)
  (let ((x (* semi-major-axis (- (cos (degrees->radians eccentric-anomaly))
                                 eccentricity)))
        (y (* semi-major-axis
              (sqrt (- 1 (expt eccentricity 2)))
              (sin (degrees->radians eccentric-anomaly))))
        (z 0))
    (values x y z)))

(define (right-ascension x-equatorial
                         y-equatorial
                         z-equatorial)
  (+ (radians->degrees (atan (/ y-equatorial x-equatorial)))
     (cond ((negative? x-equatorial) 180)
           ((and (positive? x-equatorial)
                 (negative? y-equatorial)) 360)
           (else 0))))

(define (declination x-equatorial
                     y-equatorial
                     z-equatorial)
  (radians->degrees
   (atan (/ z-equatorial
            (sqrt (+ (expt x-equatorial 2)
                     (expt y-equatorial 2)))))))

(define (mean-anomaly mean-longitude
                      longitude-perihelion
                      b
                      T
                      c
                      f
                      s)
  (let ((mean-anomaly
         (+ mean-longitude
            (- longitude-perihelion)
            (* b (expt T 2))
            (* c (cos (degrees->radians (* f T))))
            (* s (sin (degrees->radians (* f T)))))))
    (+ (modulo mean-anomaly 360)
       (if (negative? mean-anomaly)
           360
           0))))

(define (mean-anomaly-alt mean-longitude longitude-perihelion)
  (let ((mean-anomaly (- mean-longitude longitude-perihelion)))
    (+ (modulo mean-anomaly 360)
       (if (negative? mean-anomaly)
           360
           0))))

(define (allocentric-kepler target source julian-days)
  (let-values (((x-ecliptic y-ecliptic z-ecliptic
                            radius right-ascension declination)
                (kepler source 0.0 0.0 0.0 julian-days)))
    (kepler target x-ecliptic y-ecliptic z-ecliptic julian-days)))

(define (radius . equatorials)
  (sqrt (apply + (map (cut expt <> 2) equatorials))))

(define (kepler target
                source-x-ecliptic
                source-y-ecliptic
                source-z-ecliptic
                julian-days)
  (let ((T (/ (- julian-days 2451545.0) 36525)))
    (let-values (((semi-major-axis
                   eccentricity
                   inclination
                   mean-longitude
                   longitude-perihelion
                   longitude-ascending-node
                   b
                   c
                   s
                   f)
                  (compute-elements
                   T
                   (planet-semi-major-axis target)
                   (planet-eccentricity target)
                   (planet-inclination target)
                   (planet-mean-longitude target)
                   (planet-longitude-perihelion target)
                   (planet-longitude-ascending-node target)
                   (planet-b target)
                   (planet-c target)
                   (planet-s target)
                   (planet-f target))))
      (let ((argument-perihelion (- longitude-perihelion longitude-ascending-node))
            (mean-anomaly (mean-anomaly-alt mean-longitude longitude-perihelion))
            (eccentricity* (radians->degrees eccentricity))
            (tolerance 10e-6))
        (let* ((eccentric-anomaly
                (eccentric-anomaly mean-anomaly
                                   eccentricity*
                                   eccentricity
                                   tolerance))
               (true-anomaly (true-anomaly eccentricity eccentric-anomaly))
               (obliquity 23.43928))
          (let*-values (((x-heliocentric y-heliocentric z-heliocentric)
                         (heliocentrics semi-major-axis
                                        eccentric-anomaly
                                        eccentricity))
                        ((x-ecliptic y-ecliptic z-ecliptic)
                         (ecliptics argument-perihelion
                                    longitude-ascending-node
                                    inclination
                                    x-heliocentric
                                    y-heliocentric
                                    z-heliocentric
                                    source-x-ecliptic
                                    source-y-ecliptic
                                    source-z-ecliptic))
                        ((x-equatorial y-equatorial z-equatorial)
                         (equatorials x-ecliptic
                                      y-ecliptic
                                      z-ecliptic
                                      obliquity))
                        ((radius)
                         (radius x-equatorial y-equatorial z-equatorial)))
                       (let ((right-ascension
                              (right-ascension x-equatorial
                                               y-equatorial
                                               z-equatorial))
                             (declination
                              (declination x-equatorial
                                           y-equatorial
                                           z-equatorial)))
                         (values x-ecliptic y-ecliptic z-ecliptic
                                 radius right-ascension declination))))))))

(define sol
  (make-planet (make-element 1e-12 0.0)
               (make-element 0.0 0.0)
               (make-element 0.0 0.0)
               (make-element 0.0 0.0)
               (make-element 0.0 0.0)
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
               (make-element 48.33076593 -0.12534081)
               (make-element 0.0 0.0)
               (make-element 0.0 0.0)
               (make-element 0.0 0.0)
               (make-element 0.0 0.0)))

(define venus
  (make-planet (make-element 0.72333566 0.00000390)
               (make-element 0.00677672 -0.00004107)
               (make-element 3.39467605 -0.00078890)
               (make-element 181.97909950 58517.81538729)
               (make-element 131.60246718 0.00268329)
               (make-element 76.67984255 -0.27769418)
               (make-element 0.0 0.0)
               (make-element 0.0 0.0)
               (make-element 0.0 0.0)
               (make-element 0.0 0.0)))

(define earth-moon-barycenter
  (make-planet (make-element 1.00000261 0.00000562)
               (make-element 0.01671123 -0.00004392)
               (make-element -0.00001531 -0.01294668)
               (make-element 100.46457166 35999.37244981)
               (make-element 102.93768193 0.32327364)
               (make-element 0.0 0.0)
               (make-element 0.0 0.0)
               (make-element 0.0 0.0)
               (make-element 0.0 0.0)
               (make-element 0.0 0.0)))

(define mars
  (make-planet (make-element 1.52371034 0.00001847)
               (make-element 0.09339410 0.00007882)
               (make-element 1.84969142 -0.00813131)
               (make-element -4.55343205 19140.30268499)
               (make-element -23.94362959 0.44441088)
               (make-element 49.55953891 -0.29257343)
               (make-element 0.0 0.0)
               (make-element 0.0 0.0)
               (make-element 0.0 0.0)
               (make-element 0.0 0.0)))

(define jupiter
  (make-planet (make-element 5.20288700 -0.00011607)
               (make-element 0.04838624 -0.00013253)
               (make-element 1.30439695 -0.00183714)
               (make-element 34.39644051 3034.74612775)
               (make-element 14.72847983 0.21252668)
               (make-element 100.47390909 0.20469106)
               (make-element 0.0 0.0)
               (make-element 0.0 0.0)
               (make-element 0.0 0.0)
               (make-element 0.0 0.0)))

(define uranus
  (make-planet (make-element 19.18916464 -0.00196176)
               (make-element 0.04725744 -0.00004397)
               (make-element 0.77263783 -0.00242939)
               (make-element 313.23810451 428.48202785)
               (make-element 170.95427630 0.40805281)
               (make-element 74.01692503 0.04240589)
               (make-element 0.0 0.0)
               (make-element 0.0 0.0)
               (make-element 0.0 0.0)
               (make-element 0.0 0.0)))

(define neptune
  (make-planet (make-element 30.06992276 0.00026291)
               (make-element 0.00859048 0.00005105)
               (make-element 1.77004347 0.00035372)
               (make-element -55.12002969 218.45945325)
               (make-element 44.96476227 -0.32241464)
               (make-element 131.78422574 -0.00508664)
               (make-element 0.0 0.0)
               (make-element 0.0 0.0)
               (make-element 0.0 0.0)
               (make-element 0.0 0.0)))

(define pluto
  (make-planet (make-element 39.48211675 -0.00031596)
               (make-element 0.24882730 0.00005170)
               (make-element 17.14001206 0.00004818)
               (make-element 238.92903833 145.20780515)
               (make-element 224.06891629 -0.04062942)
               (make-element 110.30393684 -0.01183482)
               (make-element 0.0 0.0)
               (make-element 0.0 0.0)
               (make-element 0.0 0.0)
               (make-element 0.0 0.0)))
