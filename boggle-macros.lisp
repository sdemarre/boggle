(in-package :boggle)

(defconstant +board-num-rows+ 4)
(defconstant +board-num-columns+ 4)


(defmacro do-on-rows (row-var &body body)
  `(iter (for ,row-var from 0 to (1- +board-num-rows+))
	 ,@body))

(defmacro do-on-columns (column-var &body body)
  `(iter (for ,column-var from 0 to (1- +board-num-columns+))
	 ,@body))

(defmacro do-on-positions (position-var &body body)
  (alexandria:with-gensyms (row-var column-var)
    `(do-on-rows ,row-var
       (do-on-columns ,column-var
	 (let ((,position-var (make-position ,row-var ,column-var)))
	   ,@body)))))

(defmacro do-on-letters (letter-var boggle-board &body body)
  (alexandria:with-gensyms (position)
    `(do-on-positions ,position
       (let ((,letter-var (letter ,boggle-board ,position)))
	 ,@body))))
