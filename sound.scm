(require-extension
 syntax-case
 (srfi 1 9 11 31))
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
  allocentric-kepler
  kepler
  earth-moon-barycenter
  mercury
  venus
  mars
  jupiter
  uranus
  neptune
  pluto
  sol
  degrees->radians
  radians->degrees)
 (include "util.scm")
 (include "notes.scm")
 (include "synthesis.scm")
 (include "newton.scm")
 (include "kepler.scm"))
