(in-package #:game-engine)

(defparameter *default-font* nil)
(defparameter *draw-immediately* t)
(defparameter *layers* nil)

(defun hash-table-keys (table)
  "Returns a list containing the keys of hash table TABLE."
  (let ((keys nil))
    (maphash (lambda (k v)
	       (declare (ignore v))
	       (push k keys))
	     table)
    keys))

(defun init-graphics ()
  (load-default-font "Vera.ttf" 20))

(defun load-default-font (file-name size)
  (setf *default-font* (load-font file-name size)))

(defun load-font (file-name size)
  (sdl:initialise-font (make-instance 'sdl:ttf-font-definition
				      :size size
				      :filename (merge-pathnames file-name
								 sdl:*default-font-path*))))

(defun radians->degrees (radians)
  (/ (* radians 180) pi))

(defun draw-rect (rectangle color rotation origin)
  (gl:disable :texture-2d :blend)
  (gl:color (get-color-r color) (get-color-g color) (get-color-b color) (get-color-a color))
  (let* ((x (rectangle-x rectangle))
	 (y (rectangle-y rectangle))
	 (width (rectangle-width rectangle))
	 (height (rectangle-height rectangle))
	 
	 (x1 (- (vector-2-x origin)))
	 (x2 (- width (vector-2-x origin)))
	 (y1 (- (vector-2-y origin)))
	 (y2 (- height (vector-2-y origin))))
    
    (gl:with-pushed-matrix
      (gl:translate x y 0)
      (gl:rotate (radians->degrees rotation) 0 0 1)
      
      (gl:with-primitive :quads
	(gl:vertex x1 y1 0)
	(gl:vertex x2 y1 0)
	(gl:vertex x2 y2 0)
	(gl:vertex x1 y2 0)))))

(defun add-draw-params (layer type &rest params)
  (unless (<= 0 layer 1)
    (error "Layer depth must be more than 0 and less than 1 : ~a" layer))
  (push (cons type params) (gethash layer *layers*)))

(defun internal-draw-tex-rect (texture position source-rect color rotation origin
			       &optional (scale nil) (effects nil))
  (declare (optimize (speed 3) (safety 0)))
  (let* ((x (vector-2-x position))
	 (y (vector-2-y position))

	 (texture-width (the fixnum (cadr texture)))
	 (texture-heigth (the fixnum (caddr texture)))

	 (source-rect (if source-rect
			  source-rect
			  (make-rectangle :x 0
					  :y 0
					  :width texture-width
					  :height texture-heigth)))
	 
	 (width (rectangle-width source-rect))
	 (height (rectangle-height source-rect))

	 (translate-x (if (eq effects :flip-horizontally) (+ x width) x))
	 (translate-y (if (eq effects :flip-vertically) (+ y height) y))
	 (scale-x (if (eq effects :flip-horizontally) -1.0 1.0))
	 (scale-y (if (eq effects :flip-vertically) -1.0 1.0))
	 
	 (x1 (- (the single-float (vector-2-x origin))))
	 (x2 (- width (vector-2-x origin)))
	 (y1 (- (the single-float (vector-2-y origin))))
	 (y2 (- height (vector-2-y origin)))

	 
	 (d-tex-x (/ 1.0 (coerce texture-width 'single-float)))
	 (d-tex-y (/ 1.0 (coerce texture-heigth 'single-float)))
	 
	 (tex-x1 (* d-tex-x (coerce (the fixnum (rectangle-x source-rect)) 'single-float)))
	 (tex-x2 (* d-tex-x (coerce (+ (the fixnum (rectangle-x source-rect)) (the fixnum width)) 'single-float)))
	 (tex-y1 (* d-tex-y (rectangle-y source-rect)))
	 (tex-y2 (* d-tex-y (coerce (+ (the fixnum (rectangle-y source-rect)) (the fixnum height)) 'single-float))))

    (gl:enable :texture-2d :blend)
    ;; (gl:blend-func :one :one-minus-src-alpha)
    (gl:blend-func :src-alpha :one-minus-src-alpha)
    (gl:color (the single-float (get-color-r color))
	      (the single-float (get-color-g color))
	      (the single-float (get-color-b color))
	      (the single-float (get-color-a color)))
    (gl:bind-texture :texture-2d (car texture))

    (when (and scale (> (the single-float scale) 0))
      (let ((x-op (if (eq effects :flip-horizontally) #'+ #'-))
	    (y-op (if (eq effects :flip-vertically) #'+ #'-)))
	(setf scale-x (* scale-x scale))
	(setf scale-y (* scale-y scale))
	(setf translate-x (funcall x-op translate-x (/ (- (* width scale) width) 2)))
	(setf translate-y (funcall y-op translate-y (/ (- (* height scale) height) 2)))))
    
    (gl:with-pushed-matrix
      (gl:translate (the single-float translate-x) (the single-float translate-y) 0.0)
      (gl:scale scale-x scale-y 1)
      (gl:rotate (the single-float (radians->degrees rotation)) 0.0 0.0 1.0)
      
      (gl:with-primitive :quads
	(gl:tex-coord tex-x1 tex-y1) (gl:vertex (the single-float x1) (the single-float y1) 0.0)
	(gl:tex-coord tex-x2 tex-y1) (gl:vertex (the single-float x2) (the single-float y1) 0.0)
	(gl:tex-coord tex-x2 tex-y2) (gl:vertex (the single-float x2) (the single-float y2) 0.0)
	(gl:tex-coord tex-x1 tex-y2) (gl:vertex x1 y2 0.0)))
    (gl:disable :texture-2d :blend)))

(defun draw-tex-rect (texture position source-rect color rotation origin
		      &optional (scale nil) (effects nil) (layer-depth 0.0))
  "Draw a textured rectnagle in the specified position of the screen
    texture     - value returned by function 'load-a-texture'
    position    - value of type vector-2 struct
    source-rect - value of type rectangle struct
    color       - value of type color struct
    rotation    - value of type float or integer
    origin      - value of type vector-2 struct
    scale       - value of type float or integer"
  (if *draw-immediately*
      (internal-draw-tex-rect texture position source-rect color rotation origin scale effects)
      (add-draw-params layer-depth :textured-rectangle texture position source-rect color rotation origin scale effects)))

(defun begin-draw ()
  (setf *draw-immediately* nil)
  (setf *layers* (make-hash-table)))

(defun end-draw ()
  (setf *draw-immediately* t)
  (dolist (key (sort (hash-table-keys *layers*) #'>))
    (dolist (params (reverse (gethash key *layers*)))
      (case (car params)
	(:textured-rectangle
	 (destructuring-bind (texture position source-rect color rotation origin scale effects) (cdr params)
	   (internal-draw-tex-rect texture position source-rect color rotation origin scale effects)))
	(:string
	 (destructuring-bind (string position color scale centered font) (cdr params)
	   (draw-string string position color :scale scale :centered centered :font font))))))
  (setf *layers* nil))

(defmacro with-deferred-draw (&body body)
  `(progn
     (begin-draw)
     ,@body
     (end-draw)))

(defun draw-string (string position color &key (scale nil) (centered t) (font *default-font*) (layer-depth 0.0))
  (if *draw-immediately*
      (render-string-blended string position color :scale scale :centered centered :font font)
      (add-draw-params layer-depth :string string position color scale centered font)))

(defun render-string-blended (string position color &key (scale nil) (centered t) (font *default-font*))
  (gl:enable :texture-2d :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)

  (let* ((sdl-color (sdl:color :r (color-r color)
			       :g (color-g color)
			       :b (color-b color)
			       :a (color-a color)))
	 (message (sdl:render-string-blended string :color sdl-color :font font))
	 (texture (gl:gen-texture))
	 (width (sdl:width message))
	 (height (sdl:height message))
	 (pixels (cffi:foreign-slot-value (sdl:fp message)
					  '(:struct sdl-cffi::SDL-Surface)
					  'sdl-cffi::pixels)))
    (gl:bind-texture :texture-2d texture)

    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)

    (gl:tex-image-2d :texture-2d 0 :rgba width height 0 :bgra :unsigned-byte pixels)

    (gl:color 1 1 1 (get-color-a color))

    (let* ((x (vector-2-x position))
	   (y (vector-2-y position))
	   (width (if scale (* width scale) width))
	   (height (if scale (* height scale) height))
	   (x1 (if centered (- x (/ width 2)) x))
	   (y1 (if centered (- y (/ height 2)) y))
	   (x2 (if centered (+ x (/ width 2)) (+ x width)))
	   (y2 (if centered (+ y (/ height 2)) (+ y height))))
      (gl:with-primitive :quads
    	(gl:tex-coord 0 0) (gl:vertex x1 y1)
    	(gl:tex-coord 0 1) (gl:vertex x1 y2)
    	(gl:tex-coord 1 1) (gl:vertex x2 y2)
    	(gl:tex-coord 1 0) (gl:vertex x2 y1)))

    (gl:disable :texture-2d :blend)

    (sdl:free message)
    (gl:delete-textures (list texture))))

(defun load-a-texture (filename)
  (let ((texture (car (gl:gen-textures 1)))
	(image (sdl-image:load-image filename)))
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :generate-mipmap t)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)

    (sdl-base::with-pixel (pix (sdl:fp image))
      ;; we should probably be a bit more intelligent about this, but this
      ;; handles some common cases
      (let ((texture-format (ecase (sdl-base::pixel-bpp pix)
			      (1 :luminance)
			      (2 :luminance-alpha)
			      (3 :rgb)
			      (4 :rgba))))
	;; we should also handle this properly, by adjusting the
	;; settings of gl:pixel-store
	(assert (and (= (sdl-base::pixel-pitch pix)
			(* (sdl:width image) (sdl-base::pixel-bpp pix)))
		     (zerop (rem (sdl-base::pixel-pitch pix) 4))))
	(gl:tex-image-2d :texture-2d 0 :rgba
			 (sdl:width image) (sdl:height image)
			 0
			 texture-format
			 :unsigned-byte (sdl-base::pixel-data pix))))
    (list texture (sdl:width image) (sdl:height image))))

(defun clear-color (color)
  (gl:clear-color (get-color-r color)
		  (get-color-g color)
		  (get-color-b color)
		  (get-color-a color)))

(defun texture-width (texture)
  (cadr texture))

(defun texture-heigth (texture)
  (caddr texture))
