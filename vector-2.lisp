(in-package #:game-engine)

(defstruct vector-2
  (x 0.0 :type float)
  (y 0.0 :type float))

(defun add (&rest addends)
  "Adds values of vector-2 struct"
  (reduce 'binary-add (cdr addends) :initial-value (car addends)))

(defgeneric binary-add (augend addend))
(defgeneric distance (lhs rhs)
  (:documentation
   "lhs - type of vector-2 struct
rhs - type of vector-2 struct"))
(defgeneric multiply (lhs rhs))
(defgeneric normalize (obj))
(defgeneric divide (lhs rhs))
(defgeneric subtract (lhs rhs))
(defgeneric reflect (vector normal))
(defgeneric vector-2-length (obj))

(defmethod binary-add ((augend vector-2) (addend vector-2))
  (make-vector-2 :x (+ (vector-2-x augend) (vector-2-x addend))
		 :y (+ (vector-2-y augend) (vector-2-y addend))))

(defun square (x)
  (* x x))

(defmethod distance ((lhs vector-2) (rhs vector-2))
  (sqrt (+ (square (- (vector-2-x lhs) (vector-2-x rhs)))
	   (square (- (vector-2-y lhs) (vector-2-y rhs))))))

(defmacro defoperation (name operation type)
  `(defmethod ,name ((lhs vector-2) (rhs ,type))
     (make-vector-2 :x (,operation (vector-2-x lhs) rhs)
		    :y (,operation (vector-2-y lhs) rhs))))

(defoperation multiply * float)
(defoperation multiply * integer)
(defoperation divide / float)
(defoperation divide / integer)

(defmethod normalize ((obj vector-2))
  (if (zero? obj)
      (make-vector-2)
      (let ((length (sqrt (+ (square (vector-2-x obj)) (square (vector-2-y obj))))))
	(make-vector-2 :x (coerce (/ (vector-2-x obj) length) 'float)
		       :y (coerce (/ (vector-2-y obj) length) 'float)))))

(defun zero? (obj)
  "obj - type of vector-2 struct"
  (and (= (vector-2-x obj) 0.0)
       (= (vector-2-y obj) 0.0)))

(defmethod subtract ((lhs vector-2) (rhs vector-2))
  (make-vector-2 :x (- (vector-2-x lhs) (vector-2-x rhs))
		 :y (- (vector-2-y lhs) (vector-2-y rhs))))

(defmethod reflect ((vector vector-2) (normal vector-2))
  (let ((num (+ (* (vector-2-x vector) (vector-2-x normal))
		(* (vector-2-y vector) (vector-2-y normal)))))
    (make-vector-2 :x (- (vector-2-x vector) (* 2.0 num (vector-2-x normal)))
		   :y (- (vector-2-y vector) (* 2.0 num (vector-2-y normal))))))

(defmethod vector-2-length ((obj vector-2))
  (sqrt (+ (square (vector-2-x obj)) (square (vector-2-y obj)))))

(defun equals? (lhs rhs)
  (and (= (vector-2-x lhs) (vector-2-x rhs))
       (= (vector-2-y lhs) (vector-2-y rhs))))

(defmethod copy ((vect vector-2))
  (make-vector-2 :x (vector-2-x vect) :y (vector-2-y vect)))
