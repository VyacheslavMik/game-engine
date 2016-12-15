(in-package #:game-engine)

;; physical size of window in pixels
(defparameter *actual-screen-width* 800)
(defparameter *actual-screen-height* 600)

;; nominal size of window in pixels, in case we just want to scale the
;; scene to match the window instead of showing more of the world
(defparameter *nominal-screen-width* 800)
(defparameter *nominal-screen-height* 600)

;; extents of the window in GL coordinates
(defparameter *screen-width* nil)
(defparameter *screen-height* nil)

;; flag specifying how we want to handle changing resolution:
;; if T, always show a fixed amount of the world
;; if NIL, keep 1 pixel on screen = 1 unit of world space, so more of the
;; world shows when the window gets larger
(defparameter *use-nominal-size* t)

(defun setup-ortho-projection (width height)
  (setf *actual-screen-width* width)
  (setf *actual-screen-height* height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:viewport 0 0 width height)
  (if *use-nominal-size*
      (setf *screen-width* *nominal-screen-width*
	    *screen-height* *nominal-screen-height*)
      (setf *screen-width* *actual-screen-width*
	    *screen-height* *actual-screen-height*))
  ;; this sets up the world so the screen coordinates go from 0,0 at lower
  ;; left to width,height at uppper right
  (gl:ortho 0 *screen-width* *screen-height* 0 0 1)
  (gl:matrix-mode :modelview))


(defparameter *title-caption* "Game Engine")
(defparameter *icon-caption* "Game Engine")
(defparameter *pressed-keys* '())

(defun main-loop (init-fn draw-fn update-fn)
  (sdl:with-init ()
    (sdl:window *nominal-screen-width* *nominal-screen-height*
		:title-caption *title-caption*
		:icon-caption *icon-caption*
		:flags (logior sdl:sdl-opengl
			       sdl:sdl-resizable))
    (setf (sdl:frame-rate) 60)
    (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)
    (let ((prev-tick (sdl:sdl-get-ticks)))
      (flet ((mx (x)   ; adjust mouse coordinates from screen to world
	       (* x (/ (float *screen-width* 1.0) *actual-screen-width*)))
	     (my (y)   ; adjust mouse coordinates from screen to world
	       (* y (/ (float *screen-height* 1.0) *actual-screen-height*))))
	(setup-ortho-projection *nominal-screen-width* *nominal-screen-height*)
	(with-init-sound
	  (init-graphics)
	  (init init-fn)
	  (sdl:with-events ()
	    (:quit-event () t)
	    (:video-resize-event (:w w :h h)
				 (sdl:resize-window w h)
				 (setup-ortho-projection w h))
	    (:key-down-event (:state state :scancode scancode :key key
				     :mod-key mod-key :unicode unicode)
			     (key-down key state mod-key scancode unicode))
	    (:key-up-event (:state state :scancode scancode :key key
				   :mod-key mod-key :unicode unicode)
			   (key-up key state mod-key scancode unicode))
	    (:mouse-button-down-event (:button button :state state :x x :y y)
				      (mouse-down button state (mx x) (my y)))
	    (:idle ()
		   (let* ((ticks (sdl:sdl-get-ticks))
			  (total-seconds (/ (- ticks prev-tick) 1000)))
		     (setf prev-tick ticks)
		     (funcall update-fn (coerce total-seconds 'float))
		     (draw draw-fn)))))))))

(defun init (init-fn)
  (setf *pressed-keys* '())
  
  (apply init-fn nil))

(defun draw (draw-fn)
  (gl:clear :color-buffer-bit)

  (apply draw-fn nil)

  (sdl:update-display))

(defun mouse-down (button state x y)
  (declare (ignore button state x y)))

(defun key-up (key state mod-key scancode unicode)
  (declare (ignore state mod-key scancode unicode))
  (if (eql key :sdl-key-escape)
      (sdl:push-quit-event)
      (setf *pressed-keys* (remove key *pressed-keys*))))

(defun key-down (key state mod-key scancode unicode)
  (declare (ignore state mod-key scancode unicode))
  (if (eql key :sdl-key-escape)
      (sdl:push-quit-event)
      (push key *pressed-keys*)))

(defun key-pressed? (key)
  (member key *pressed-keys*))

(defun get-mouse-x ()
  (sdl:mouse-x))

(defun get-mouse-y ()
  (sdl:mouse-y))

(defun mouse-left-button-pressed? ()
  (sdl:mouse-left-p))

(defun mouse-right-button-pressed? ()
  (sdl:mouse-right-p))
