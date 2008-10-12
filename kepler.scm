(define obliquity 0.409092610296857)

(define tolerance 10e-6)

(define (radians->degrees . radians)
  (apply values (map (cut * <> (/ 180.0 pi)) radians)))

(define (degrees->radians . degrees)
  (apply values (map (cut * <> (/ pi 180.0)) degrees)))

(define (degrees->hours . degrees)
  (apply values (map (cut / <> 15) degrees)))

(define (hours->degrees . hours)
  (apply values (map (cut * <> 15) hours)))

(define (radians->hours . radians)
  (apply values
         (map (lambda (radians)
                (degrees->hours (radians->degrees radians)))
              radians)))

(define (hms->hours hours minutes seconds)
  (+ hours (/ minutes 60) (/ seconds 60)))

(define (dms->degrees degrees minutes seconds)
  (if (positive? degrees)
      (+ degrees (/ minutes 60) (/ seconds 60))
      (- degrees (/ minutes 60) (/ seconds 60))))

(define (dms->radians degrees minutes seconds)
  (degrees->radians (dms->degrees degrees minutes seconds)))

(define (hours->radians . hours)
  (apply values
         (map (lambda (hours) (degrees->radians (hours->degrees hours)))
              hours)))

(define (hms->radians hours minutes seconds)
  (hours->radians (hms->hours hours minutes seconds)))

(define (ecliptics true-anomaly
                   heliocentric-radius
                   longitude-ascending-node
                   longitude-perihelion
                   inclination
                   source-x-ecliptic
                   source-y-ecliptic
                   source-z-ecliptic)
  (let ((internal-term (+ true-anomaly
                          longitude-perihelion
                          (- longitude-ascending-node))))
    (let ((cos-longitude-ascending-node (cos longitude-ascending-node))
          (sin-longitude-ascending-node (sin longitude-ascending-node))
          (cos-internal-term (cos internal-term))
          (sin-internal-term (sin internal-term))
          (cos-inclination (cos inclination))
          (sin-inclination (sin inclination)))
      (let ((x (* heliocentric-radius (- (* cos-longitude-ascending-node
                                            cos-internal-term)
                                         (* sin-longitude-ascending-node
                                            sin-internal-term
                                            cos-inclination))))
            (y (* heliocentric-radius (+ (* sin-longitude-ascending-node
                                            cos-internal-term)
                                         (* cos-longitude-ascending-node
                                            sin-internal-term
                                            cos-inclination))))

            (z (* heliocentric-radius
                  sin-internal-term
                  sin-inclination)))
        (values (- x source-x-ecliptic)
                (- y source-y-ecliptic)
                (- z source-z-ecliptic))))))

(define (heliocentric-radius semi-major-axis eccentricity true-anomaly)
  (/ (* semi-major-axis (- 1 (expt eccentricity 2)))
     (+ 1 (* eccentricity (cos true-anomaly)))))

(define (true-anomaly eccentricity eccentric-anomaly)
  (modulo
   (* 2 (atan (* (sqrt (/ (+ 1 eccentricity)
                          (- 1 eccentricity)))
                 (tan (* 0.5 eccentric-anomaly)))))
   (* 2 pi)))


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
  (atan (/ z-equatorial
           (sqrt (+ (expt x-equatorial 2)
                    (expt y-equatorial 2))))))

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
                                   tolerance))
               (true-anomaly (true-anomaly eccentricity eccentric-anomaly))
               (heliocentric-radius
                (heliocentric-radius semi-major-axis eccentricity true-anomaly)))
          (let*-values (((x-ecliptic y-ecliptic z-ecliptic)
                         (ecliptics true-anomaly
                                    heliocentric-radius
                                    longitude-ascending-node
                                    longitude-perihelion
                                    inclination
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
