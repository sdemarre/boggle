(in-package :boggle)

(defun start-html-server ()
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4242)))

(defun to-letters (letters)
  (if (and letters (stringp letters) (= (length letters) 16))
      letters
      (random-boggle-string)))

(defun cell-id (row col)
  (format nil "c~a:~a" row col))
(defun boggle-board-html (board)
  (cl-who:with-html-output-to-string (s nil :indent t)
    (:table :border 2 :cellpadding 2 :id "board"
	    (loop for row from 0 to 3 do
		 (cl-who:htm (:tr
			      (loop for column from 0 to 3 do
				   (cl-who:htm (:td :id (cell-id row column)
						    (:svg :width 40 :height 40 :fill "red"
							  (:text :font-size 20 :y 25 :x 15
								 :style "not-highlighted"
								 (cl-who:str (format nil "~a" (string-upcase (letter board (make-position row column))))))))))))))))
(defun make-highlight-fun (solution)
  (let ((cells (iter (for (row . col) in (cadr solution))
		     (collect `(array ,row ,col)))))
    (eval `(parenscript:ps (highlight-cells (array ,@cells))))))
(defun solutions-html (solutions)
  (cl-who:with-html-output-to-string (s nil :indent t)
    (:table
     (loop for solution in solutions do
	  (cl-who:htm (:tr
		       (:td (cl-who:str (car solution)))
		       (:td :onClick (make-highlight-fun solution)
			    (cl-who:str (cadr solution)))))))))
(hunchentoot:define-easy-handler (run-boggle :uri "/run") (letters language)
  (let ((board (make-board-from-string (to-letters letters)))
	(words (if (and language (stringp language) (string= language "en"))
		   (read-english-words)
		   (read-dutch-words))))
    (cl-who:with-html-output-to-string (s nil :indent t)
      (:html
       (:head (:style (cl-who:str "table#board, td#board { text-align: center;font-size: xx-large}
				  .highlighted {background=green}
				  .not-highlighted {background=white}"))
	      (:script (cl-who:str
			(parenscript:ps
			  (defun cell-id (row column)
			    (let ((cell-id "c"))
			      (parenscript:chain cell-id (concat row ":" column))))
			  (defun highlight-cells (cells)
			    (loop for row from 0 to 3 do
				 (loop for col from 0 to 3 do
				      (setf (parenscript:chain document (get-element-by-id (cell-id row col)) style) :not-highlighted)))
			    (loop for cell in cells do
				 (setf (parenscript:chain document (get-element-by-id (cell-id (aref cell 0) (aref  cell 1))) style) :highlighted))))))
	      (:body
	       (cl-who:str (boggle-board-html board))
	       (cl-who:str (solutions-html (solve-boggle-board board words)))))))))


