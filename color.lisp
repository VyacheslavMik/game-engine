(in-package #:game-engine)

(defparameter *bitmask* 65536)

(defstruct color
  (r 0 :type integer)
  (g 0 :type integer)
  (b 0 :type integer)
  (a 255 :type integer))

(defgeneric get-color-r (color))
(defgeneric get-color-g (color))
(defgeneric get-color-b (color))
(defgeneric get-color-a (color))
(defgeneric scale-color (color scale))
(defgeneric lerp-color (color1 color2 amount))

(defmacro fraction->float (sexp)
  `(coerce ,sexp 'float))

(defmacro define-get-color-parm (parm)
  (let* ((color-parm (concatenate 'string "COLOR-" (symbol-name parm)))
	 (color-parm-func (intern color-parm))
	 (fun-name (intern (concatenate 'string "GET-" color-parm)))
	 (color (gensym)))
    `(defmethod ,fun-name ((,color color))
       (fraction->float (/ (,color-parm-func ,color) 255)))))


(define-get-color-parm r)
(define-get-color-parm g)
(define-get-color-parm b)
(define-get-color-parm a)

(defun make-white-color ()
  (make-color :r 255 :g 255 :b 255))

(defun make-yellow-color ()
  (make-color :r 255 :g 255 :b 0))

(defun make-wheat-color ()
  (make-color :r 245 :g 222 :b 179))

(defun make-white-smoke-color ()
  (make-color :r 245 :g 245 :b 245))

(defun make-slate-gray-color ()
  (make-color :r 112 :g 128 :b 144))

(defun make-orange-color ()
  (make-color :r 255 :g 165 :b 0 :a 255))

(defun make-black-color ()
  (make-color))

(defmethod scale-color ((color color) scale)
  (flet ((scale-parm (parm scale)
		  (let ((value (* parm scale)))
		    (cond ((< value 0) 0)
			  ((> value 255) 255)
			  (t (floor value))))))
    (make-color :r (scale-parm (color-r color) scale)
		:g (scale-parm (color-g color) scale)
		:b (scale-parm (color-b color) scale)
		:a (scale-parm (color-a color) scale))))

(defmethod lerp-color ((color1 color) (color2 color) amount)
  (let ((k (round (max 0 (min (* *bitmask* amount) *bitmask*))))
	(r1 (color-r color1))
	(g1 (color-g color1))
	(b1 (color-b color1))
	(a1 (color-a color1))
	(r2 (color-r color2))
	(g2 (color-g color2))
	(b2 (color-b color2))
	(a2 (color-a color2)))
    (make-color :r (+ r1 (ash (* (- r2 r1) k) -16))
		:g (+ g1 (ash (* (- g2 g1) k) -16))
		:b (+ b1 (ash (* (- b2 b1) k) -16))
		:a (+ a1 (ash (* (- a2 a1) k) -16)))))
