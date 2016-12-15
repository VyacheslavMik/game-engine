(in-package #:game-engine)

(defconstant +two-pi+ (* 2 pi))

(defun clamp (value min max)
  (max min (min value max)))

(defun degrees->radians (degrees)
  (/ (* pi degrees) 180))
