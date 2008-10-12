(define obliquity 0.409092610296857)

(define tolerance 10e-6)

(define (radians->degrees . radians)
  (apply values (map (cut * <> (/ 180.0 pi)) radians)))

(define (degrees->radians . degrees)
  (apply values (map (cut * <> (/ pi 180.0)) degrees)))

(define (degrees->minutes . degrees)
  (apply values (map (cut / <> 15) degrees)))

(define (ecliptics argument-perihelion
                   longitude-ascending-node
                   inclination
                   x-heliocentric
                   y-heliocentric
                   z-heliocentric
                   source-x-ecliptic
                   source-y-ecliptic
                   source-z-ecliptic)
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
            (- z source-z-ecliptic))))

(define (equatorials x-ecliptic
                     y-ecliptic
                     z-ecliptic
                     obliquity)
  (let ((x-equatorial x-ecliptic)
        (y-equatorial (+ (* (cos obliquity)
                            y-ecliptic)
                         (- (* (sin obliquity)
                               z-ecliptic))))
        (z-equatorial (+ (* (sin obliquity)
                            y-ecliptic)
                         (* (cos obliquity)
                            z-ecliptic))))
    (values x-equatorial y-equatorial z-equatorial)))

(define (compute-element element T)
  (+ (element-j2000 element) (* (element-rate element) T)))

(define (compute-elements T . elements)
  (apply values (map (cut compute-element <> T) elements)))

(define (eccentric-anomaly mean-anomaly eccentricity tolerance)
  (let iter ((eccentric-anomaly
              (+ mean-anomaly
                 (* eccentricity
                    (sin mean-anomaly)))))
    (let* ((delta-mean-anomaly
            (- mean-anomaly
               (- eccentric-anomaly
                  (* eccentricity
                     (sin eccentric-anomaly)))))
           (delta-eccentric-anomaly
            (/ delta-mean-anomaly
               (- 1 (* eccentricity
                       (cos eccentric-anomaly))))))
      (if (< (abs delta-eccentric-anomaly) tolerance)
          eccentric-anomaly
          (iter (+ eccentric-anomaly delta-eccentric-anomaly))))))

(define (heliocentrics semi-major-axis eccentric-anomaly eccentricity)
  (let ((x (* semi-major-axis (- (cos eccentric-anomaly)
                                 eccentricity)))
        (y (* semi-major-axis
              (sqrt (- 1 (expt eccentricity 2)))
              (sin eccentric-anomaly)))
        (z 0))
    (values x y z)))

(define (right-ascension x-equatorial
                         y-equatorial
                         z-equatorial)
  (+ (atan (/ y-equatorial x-equatorial))
     (cond ((negative? x-equatorial) pi)
           ((and (positive? x-equatorial)
                 (negative? y-equatorial)) (* 2 pi))
           (else 0))))

(define (declination x-equatorial
                     y-equatorial
                     z-equatorial)
  (radians->degrees
   (atan (/ z-equatorial
            (sqrt (+ (expt x-equatorial 2)
                     (expt y-equatorial 2)))))))

(define (mean-anomaly mean-longitude longitude-perihelion)
  (let ((mean-anomaly (- mean-longitude longitude-perihelion)))
    (+ (modulo mean-anomaly (* 2 pi))
       (if (negative? mean-anomaly)
           (* 2 pi)
           0))))

(define (delta . equatorials)
  (sqrt (apply + (map (cut expt <> 2) equatorials))))

(define (allocentric-kepler target source julian-days)
  (let-values (((x-ecliptic y-ecliptic z-ecliptic
                            delta right-ascension declination)
                (kepler source 0.0 0.0 0.0 julian-days)))
    (kepler target x-ecliptic y-ecliptic z-ecliptic julian-days)))

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
                   longitude-ascending-node)
                  (compute-elements
                   T
                   (planet-semi-major-axis target)
                   (planet-eccentricity target)
                   (planet-inclination target)
                   (planet-mean-longitude target)
                   (planet-longitude-perihelion target)
                   (planet-longitude-ascending-node target))))
      (let ((argument-perihelion (- longitude-perihelion longitude-ascending-node))
            (mean-anomaly (mean-anomaly mean-longitude longitude-perihelion)))
        (let* ((eccentric-anomaly
                (eccentric-anomaly mean-anomaly
                                   eccentricity
                                   tolerance)))
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
                        ((delta)
                         (delta x-equatorial y-equatorial z-equatorial)))
                       (let ((right-ascension
                              (right-ascension x-equatorial
                                               y-equatorial
                                               z-equatorial))
                             (declination
                              (declination x-equatorial
                                           y-equatorial
                                           z-equatorial)))
                         (values x-ecliptic y-ecliptic z-ecliptic
                                 delta right-ascension declination))))))))
