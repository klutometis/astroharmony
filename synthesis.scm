(define pi (* (atan 1) 4))

(define (dB n)
  (expt 10 (exact->inexact (/ n 10))))

(define dB-3 0.501187233627272)

(define (pure-tone rate pitch size volume)
  (let ((scale (/ (* 2 pi) rate))
        (amplitude (* volume (expt 2 (- size 1)))))
    (lambda (t) (* amplitude (sin (* t scale pitch))))))

(define (adder . sounds)
  (lambda (t) (apply + (map (cut <> t) sounds))))

(define (stereo-decomposition t mixer sounds)
  (let-values (((left right)
                (let iter ((sounds sounds)
                               (left-channels '())
                               (right-channels '()))
                      (if (null? sounds)
                          (values (apply mixer left-channels)
                                  (apply mixer right-channels))
                          (let-values (((left-channel right-channel)
                                        ((car sounds) t)))
                            (iter (cdr sounds)
                                  (cons left-channel left-channels)
                                  (cons right-channel right-channels)))))))
    (values left right)))

(define (stereo-adder . sounds)
  (lambda (t)
    (stereo-decomposition t adder sounds)))

(define (stereo-mixer . sounds)
  (lambda (t)
    (stereo-decomposition t mixer sounds)))

(define (amplifier sound factor)
  (lambda (t) (* (sound t) factor)))

(define (dampener sound factor)
  (amplifier sound (/ 1 factor)))

(define (mixer . sounds)
  (let* ((n (length sounds))
         (dampened-sounds (map (cut dampener <> n) sounds)))
    (apply adder dampened-sounds)))

;;; Applicative (greedy) vs. normal (lazy) order: that is, generate
;;; the sound, then split; as opposed to split the sound, then
;;; generate. Has consequences for noise: noise applied in normal
;;; order will be different across channels; noise applied in
;;; applicative order will be the same across channels.
(define (greedy-splitter sound n)
  (lambda (t)
    (let ((sample (sound t)))
      (apply values (make-list n sample)))))

;;; Not state-safe
(define (lazy-splitter sound n)
  (apply values (make-list n sound)))

;;; Normal (lazy) order: predigestion into channels; shouldn't pan
;;; push either channel elsewhere, irrespective of the other?
(define (pan l r law rotation)
  (let ((l (amplifier l law))
        (r (amplifier r law)))
    (cond ((zero? rotation) (values l r))
          ((negative? rotation)
           (values (adder l (amplifier r (abs rotation)))
                   (amplifier r (+ 1 rotation))))
          (else
           (values (amplifier l (- 1 rotation))
                   (adder (amplifier l rotation) r))))))

(define (balance l r bias)
  (cond ((zero? bias) (values l r))
        ((negative? bias)
         (values l (amplifier r (+ 1 bias))))
        (else
         (values (amplifier l (- 1 bias)) r))))

(define (period min max offset duration)
  (let* ((range (/ (- max min) 2))
         (pivot (+ min range)))
    (let ((scale (/ (* 2 pi) duration)))
      (lambda (t)
        (let ((value
               (+ pivot (* range (sin (+ (* t scale) offset))))))
          value)))))

(define (periodic-pan l r law period)
  (lambda (t) (pan l r law (period t))))

(define (periodic-balance l r period)
  (lambda (t)
    (balance l r (period t))))

(define (silence t) 0)

(define (interval sound begin end)
  (lambda (t)
    (if (and (>= t begin)
             (< t end))
        (sound t)
        0)))

(define (interval/silence sound begin end silence)
  (lambda (t)
    (if (and (>= t begin)
             (< t end))
        (sound t)
        (silence t))))

(define (logarithmic-in-fader sound begin end)
  (let ((duration (- end begin)))
    (interval
     (lambda (t)
       ((amplifier
         sound
         (log (+ 1 (* (- t begin) (/ (- (exp 1) 1) duration))))) t))
     begin
     end)))

(define (logarithmic-out-fader sound begin end)
  (let ((duration (- end begin)))
    (interval
     (lambda (t)
       ((amplifier
         sound
         (log (- (exp 1) (* (- t begin) (/ (- (exp 1) 1) duration))))) t))
     begin
     end)))

(define (logarithmic-fader sound fade-duration total-duration)
  (adder (logarithmic-in-fader sound 0 fade-duration)
         (interval sound fade-duration (- total-duration fade-duration))
         (logarithmic-out-fader
          sound
          (- total-duration fade-duration)
          total-duration)))

(define (keplerian-panner-fader sound target source julian-zero days duration)
  (let ((delta-julian (/ days duration)))
    (lambda (t)
      (let-values (((x-ecliptic y-ecliptic z-ecliptic
                                delta right-ascension declination)
                    (allocentric-kepler target
                                        source
                                        (+ julian-zero (* t delta-julian)))))
        (pan (amplifier sound (/ 1 delta))
             silence
             dB-3
             (/ (+ 1 (sin right-ascension)) 2))))))

(define (keplerian-panner-faders source
                                 julian-zero
                                 days
                                 hertz
                                 size
                                 seconds
                                 volume
                                 targets)
  (let* ((duration (* hertz seconds))
         (sounds (map (lambda (target)
                        (keplerian-panner-fader
                         (pure-tone hertz (planet-tone target) size volume)
                         target
                         source
                         julian-zero
                         days
                         duration))
                     targets)))
    (apply stereo-mixer sounds)))

(define (sampled-keplerian-panner-fader sound target)
  (lambda (t x y z)
    (let-values (((x-ecliptic y-ecliptic z-ecliptic
                              delta right-ascension declination)
                  (kepler target
                          x
                          y
                          z
                          t)))
      (if (zero? (modulo (inexact->exact t) 100)) (debug t))
;;;       (debug (/ 1 delta))
      (pan (amplifier sound (log (/ 1 delta)))
           silence
           dB-3
           (/ (+ 1 (sin right-ascension)) 2)))))

