
;; this is parenscript, not lisp!

(defun cell-id (row column)
  (parenscript:chain "c" (concat row ":" column)))

(defun interpolate (x1 x2 fx1 fx2 x)
  (/ (- (* fx1 x2) (* fx2 x1) (* x (- fx1 fx2))) (- x2 x1)))

(defun interpolate-color (from to steps step )
  (loop for idx from 0 to 2 collect
       (parenscript:chain |Math| (round (interpolate 1 steps (aref from idx) (aref to idx) step)))))

(defun byte-to-hex-str (byte)
  (let ((hex (parenscript:chain byte (to-string 16))))
    (if (= (parenscript:chain hex length) 1)
	(parenscript:chain "0" (concat hex))
	hex)))
(defun color-to-str (color)
  (parenscript:chain "#" (concat
			  (byte-to-hex-str (aref color 0))
			  (byte-to-hex-str (aref color 1))
			  (byte-to-hex-str (aref color 2)))))

(defun highlight-cell (row column color)
  (setf (parenscript:chain document (get-element-by-id (cell-id row column)) style background) (color-to-str color))
  (setf (parenscript:chain document (get-element-by-id (cell-id row column)) children 0 style fill) :white))

(defun unhighlight-cell (row column)
  (setf (parenscript:chain document (get-element-by-id (cell-id row column)) style background) :white)
  (setf (parenscript:chain document (get-element-by-id (cell-id row column)) children 0 style fill) :black))

(defun highlight-cells (cells)
  (loop for row from 0 to 3 do
       (loop for column from 0 to 3 do
	    (unhighlight-cell row column)))
  (let ((c 0)) 
      (loop for cell in cells do
	(highlight-cell (aref cell 0) (aref cell 1) (interpolate-color '(0 255 0) '(255 0 0) (parenscript:chain cells length) (incf c))))))
