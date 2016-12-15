(in-package #:game-engine)

(defstruct rectangle
  (height 0 :type integer)
  (width  0 :type integer)
  (x      0 :type integer)
  (y      0 :type integer))

(defgeneric intersects? (rect-a rect-b)
  (:documentation "rect-a - type of rectangle struct
rect-b - type of rectnangle struct"))
(defgeneric rectangle-left (rect))
(defgeneric rectangle-top (rect))
(defgeneric rectangle-right (rect))
(defgeneric rectangle-bottom (rect))
(defgeneric rectangle-offset (rect offset-x offset-y))
(defgeneric rectangle-equal (rect1 rect2))
(defgeneric rectangle-contains (rect other))

(defmethod intersects? ((rect-a rectangle) (rect-b rectangle))
  (let* ((rect-a-x1 (rectangle-x rect-a))
	 (rect-a-x2 (+ rect-a-x1 (rectangle-width rect-a)))
	 (rect-a-y1 (rectangle-y rect-a))
	 (rect-a-y2 (+ rect-a-y1 (rectangle-height rect-a)))

	 (rect-b-x1 (rectangle-x rect-b))
	 (rect-b-x2 (+ rect-b-x1 (rectangle-width rect-b)))
	 (rect-b-y1 (rectangle-y rect-b))
	 (rect-b-y2 (+ rect-b-y1 (rectangle-height rect-b))))
    (and (< rect-a-x1 rect-b-x2)
	 (> rect-a-x2 rect-b-x1)
	 (< rect-a-y1 rect-b-y2)
	 (> rect-a-y2 rect-b-y1))))

(defmethod rectangle-right ((rect rectangle))
  (with-slots (x width) rect
    (+ x width)))

(defmethod rectangle-bottom ((rect rectangle))
  (with-slots (y height) rect
    (+ y height)))

(defmethod rectangle-left ((rect rectangle))
  (rectangle-x rect))

(defmethod rectangle-top ((rect rectangle))
  (rectangle-y rect))

(defmethod rectangle-offset ((rect rectangle) offset-x offset-y)
  (make-rectangle :x (+ (rectangle-x rect) offset-x)
		  :y (+ (rectangle-y rect) offset-y)
		  :width (rectangle-width rect)
		  :height (rectangle-height rect)))

(defmethod rectangle-equal ((rect1 rectangle) (rect2 rectangle))
  (and (= (rectangle-x rect1) (rectangle-x rect2))
       (= (rectangle-y rect1) (rectangle-y rect2))
       (= (rectangle-width rect1) (rectangle-width rect2))
       (= (rectangle-height rect1) (rectangle-height rect2))))

(defmethod rectangle-contains ((rect rectangle) (vec vector-2))
  (and (<= (rectangle-left rect) (vector-2-x vec) (rectangle-right rect))
       (<= (rectangle-top rect) (vector-2-y vec) (rectangle-bottom rect))))

(defmethod copy ((rect rectangle))
  (make-rectangle :x (rectangle-x rect)
		  :y (rectangle-y rect)
		  :width (rectangle-width rect)
		  :height (rectangle-height rect)))
