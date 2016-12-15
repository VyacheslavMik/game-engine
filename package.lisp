(in-package #:cl-user)

(defpackage #:game-engine
  (:use #:cl)
  (:export
   ;; core params
   #:*nominal-screen-width*
   #:*nominal-screen-height*
   #:*title-caption*
   #:*icon-caption*

   ;; core functions
   #:main-loop
   #:key-pressed?
   #:get-mouse-x
   #:get-mouse-y
   #:mouse-left-button-pressed?
   #:mouse-right-button-pressed?

   ;; graphics functions
   #:load-a-texture
   #:draw-tex-rect
   #:draw-rect
   #:draw-string
   #:load-default-font
   #:clear-color
   #:texture-width
   #:texture-heigth

   ;; graphics macros
   #:with-deferred-draw

   ;; sound functions
   #:load-file
   #:play-file

   ;; vector-2
   #:vector-2

   ;; vector-2 functions
   #:make-vector-2
   #:vector-2-x
   #:vector-2-y
   #:add
   #:distance
   #:multiply
   #:normalize
   #:divide
   #:subtract
   #:reflect
   #:vector-2-length
   #:zero?
   #:equals?

   ;; rectangle
   #:rectangle

   ;; rectangle functions
   #:make-rectangle
   #:rectangle-x
   #:rectangle-y
   #:rectangle-width
   #:rectangle-height
   #:intersects?
   #:rectangle-right
   #:rectangle-bottom
   #:rectangle-left
   #:rectangle-top
   #:rectangle-offset
   #:rectangle-equal
   #:rectangle-contains
   #:copy

   ;; color functions
   #:make-color
   #:make-white-color
   #:make-yellow-color
   #:make-wheat-color
   #:make-white-smoke-color
   #:make-slate-gray-color
   #:make-orange-color
   #:make-black-color
   #:get-color-r
   #:get-color-g
   #:get-color-b
   #:get-color-a
   #:color-r
   #:color-g
   #:color-b
   #:color-a
   #:scale-color
   #:lerp-color

   ;; math-helper constants
   #:+two-pi+

   ;; math-helper functions
   #:clamp
   #:degrees->radians

   ;; utils functions
   #:->int
   #:->float
   #:->vector-2
   #:copy
   #:random-range))
