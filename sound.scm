(require-extension
 syntax-case
 numbers
 (srfi 1 9 11 19 31))
(module
 sound
 (pi
  dB
  pure-tone
  adder
  amplifier
  dampener
  mixer
  greedy-splitter
  lazy-splitter
  pan
  balance
  period
  periodic-pan
  periodic-balance
  silence
  debug
  step
  octave
  a
  ais
  bes
  b
  ces
  c
  bis
  cis
  des
  d
  dis
  es
  e
  fes
  f
  eis
  fis
  ges
  g
  gis
  aes
  newton
  logarithmic-fader
  keplerian-panner-fader
  allocentric-kepler
  kepler
  earth-moon-barycenter
  mercury
  venus
  mars
  jupiter
  saturn
  uranus
  neptune
  pluto
  sol
  degrees->radians
  degrees->minutes
  radians->degrees
  planet-tone
  planet-orbit
  orbit->hertz)
 (include "util.scm")
 (include "notes.scm")
 (include "synthesis.scm")
 (include "newton.scm")
 (include "planets.scm")
 (include "kepler.scm"))
