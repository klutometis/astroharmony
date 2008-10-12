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
;;; dwarf planet; translated from degrees->radians.
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
               (make-element 0.122259947932126 -0.000103803282729438)
               (make-element 4.40259868429583 2608.79030501053)
               (make-element 1.35189357642502 0.00280085010386076)
               (make-element 0.843530995489199 -0.00218760982161663)
               87.9691
               28))

(define venus
  (make-planet (make-element 0.72333566 0.00000390)
               (make-element 0.00677672 -0.00004107)
               (make-element 0.0592482741110957 -1.37689024689833e-05)
               (make-element 3.17613445608937 1021.32854958241)
               (make-element 2.29689635603878 4.68322452858386e-05)
               (make-element 1.33831572240834 -0.00484667775462579)
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
               (make-element 0.0227660215304719 -3.20641418200886e-05)
               (make-element 0.600331137865858 52.966311891386)
               (make-element 0.257060466847075 0.00370929031433238)
               (make-element 1.75360052596996 0.00357253294639726)
               4331.572
               33))

(define saturn
  (make-planet (make-element 9.53667594 -0.00125060)
               (make-element 0.05386179 -0.00050991)
               (make-element 0.0433887433093108 3.3791145114937e-05)
               (make-element 0.87186603715888 21.3365387887055)
               (make-element 1.61615531016306 -0.00731244366619248)
               (make-element 1.9837835429754 -0.00503838053087464)
               10832.327
               34))

(define uranus
  (make-planet (make-element 19.18916464 -0.00196176)
               (make-element 0.04725744 -0.00004397)
               (make-element 0.0134850740589642 -4.2400854315025e-05)
               (make-element 5.4670362664056 7.47842217160454)
               (make-element 2.98371499179911 0.00712186505651484)
               (make-element 1.2918390439753 0.000740122402738538)
               30799.095
               35))

(define neptune
  (make-planet (make-element 30.06992276 0.00026291)
               (make-element 0.00859048 0.00005105)
               (make-element 0.0308930864549255 6.17357863015434e-06)
               (make-element -0.962026001887529 3.81283674131913)
               (make-element 0.784783148988019 -0.00562719702463221)
               (make-element 2.30006864135446 -8.87786158636444e-05)
               60190
               36))

(define pluto
  (make-planet (make-element 39.48211675 -0.00031596)
               (make-element 0.24882730 0.00005170)
               (make-element 0.299149644278536 8.40899633610868e-07)
               (make-element 4.17009839748223 2.53435429946188)
               (make-element 3.91074034063606 -0.000709117152175635)
               (make-element 1.92516687576987 -0.000206556575380875)
               90613.3055
               37))
