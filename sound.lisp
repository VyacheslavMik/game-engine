(in-package #:game-engine)

(defparameter *buffers* (make-hash-table))
(defparameter *sources* '())
(defparameter *sound-init?* nil)

(defun init-sound ()
  (unless *sound-init?*
    (unless (alut:init)
      (check-alut-error))
    (setf *sound-init?* t))
  *sound-init?*)

(defun destroy-sound ()
  (when *sound-init?*
    (maphash (lambda (key value)
	       (declare (ignore key))
	       (when (and value (al:bufferp value))
		 (check-al-error)
		 (al:delete-buffer value)
		 (check-al-error)))
	     *buffers*)
    (map nil (lambda (source)
	       (when (and source (al:sourcep source))
		 (check-al-error)
		 (al:source-stop source)
		 (check-al-error)
		 (al:delete-source source)
		 (check-al-error)))
	 *sources*)
    (setf *buffers* (make-hash-table))
    (setf *sources* '())

    (do ((res nil (alut:exit))
	 (error nil (alut:get-error))
	 (count 0 (incf count)))
	((or (and res (eql error :no-error))
	     (> count 6)))
      (when (> count 5)
	(warn "Alut exit failed")))
    (setf *sound-init?* nil))
  t)

(defun load-file (filename)
  (let ((source (al:gen-source)))
    (check-al-error)
    (push source *sources*)
    (al:source source :buffer (get-buffer filename))
    (check-al-error)
    source))

(defun get-buffer (filename)
  (multiple-value-bind (value win) (gethash filename *buffers*)
    (if win
	value
	(load-buffer filename))))

(defun load-buffer (filename)
  (let ((buffer (alut:create-buffer-from-file filename)))
    (check-alut-error)
    (setf (gethash filename *buffers*) buffer)
    buffer))

(defun play-file (source)
  (al:source-play source)
  (check-al-error))

(defun check-al-error ()
  (let ((error (al:get-error)))
    (unless (eql error :no-error)
      (error (concatenate 'string "AL: " (write-to-string error))))))

(defun check-alut-error ()
  (let ((error (alut:get-error)))
    (unless (eql error :no-error)
      (error (concatenate 'string "ALUT: " (write-to-string error))))))

(defmacro with-init-sound (&body body)
  `(unwind-protect
	(init-sound)
     (progn
       ,@body
       (destroy-sound))))
