(in-package :boggle)

(defmacro verify (expression)
  `(let ((result ,expression))
     (if (not result)
	 (format t "FAILED: ~a~%" ',expression))))
