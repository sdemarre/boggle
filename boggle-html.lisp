(in-package :boggle)

(shadowing-import 'cl-who:htm)
(shadowing-import 'cl-who:str)


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
		 (htm (:tr
		       (loop for column from 0 to 3 do
			    (htm (:td :id (cell-id row column)
				      :letter (string-upcase (letter board (make-position row column)))
				      :style "not-highlighted"
				      (:svg :width 40 :height 40
					    (:text :font-size 20 :y 25 :x 15
						   (str (string-upcase (letter board (make-position row column)))))))))))))))
(defun make-highlight-fun (solution)
  (let ((cells (iter (for (row . col) in (cadr solution))
		     (collect `(array ,row ,col)))))
    (eval `(parenscript:ps (highlight-cells (array ,@cells))))))
(defun solutions-html (solutions)
  (cl-who:with-html-output-to-string (s nil :indent t)
    (:table
     (loop for solution in solutions do
	  (htm (:tr :onClick (make-highlight-fun solution)
		(:td (str (car solution)))
		(:td (str (cadr solution)))))))))
(hunchentoot:define-easy-handler (run-boggle :uri "/run") (letters language)
  (let ((board (make-board-from-string (to-letters letters)))
	(words (if (and language (stringp language) (string= language "en"))
		   (read-english-words)
		   (read-dutch-words))))
    (cl-who:with-html-output-to-string (s nil :indent t)
      (:html
       (:head (:style (str "table#board, td#board { text-align: center;font-size: xx-large}
				  .highlighted {background=green}
				  .not-highlighted {background=white}"))
	      (:script (str
			(parenscript:ps-compile-file "boggle-ps-highlighting.lisp")))
	      (:body
	       (str (boggle-board-html board))
	       (str (solutions-html (solve-boggle-board board words)))))))))


