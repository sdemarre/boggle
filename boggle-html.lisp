(in-package :boggle)

(defun start-html-server ()
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4242))
  (push (hunchentoot:create-prefix-dispatcher "/en" 'handle-en-board) hunchentoot:*dispatch-table*)
  (push (hunchentoot:create-prefix-dispatcher "/nl" 'handle-nl-board) hunchentoot:*dispatch-table*))

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
				      :letter (string-upcase (letter board (make-position row column)))
				      (:svg :width 40 :height 40
					    (:text :font-size 20 :y 25 :x 15
						   (cl-who:str (string-upcase (letter board (make-position row column)))))))))))))))
(defun make-highlight-fun (solution)
  (let ((cells (iter (for (row . col) in (cadr solution))
		     (collect `(array ,row ,col)))))
    (parenscript:ps* `(highlight-cells (array ,@cells)))))
(defun solutions-html (solutions)
  (cl-who:with-html-output-to-string (s nil :indent t)
    (:table :id "words"
     (loop for solution in solutions do
	  (cl-who:htm (:tr :onClick (make-highlight-fun solution)
		    :onMouseover (make-highlight-fun solution)
		    :onMouseLeave (parenscript:ps (unhighlight-all-cells))
		(:td (cl-who:str (car solution)))
		(:td (cl-who:str (cadr solution)))))))))
(defun create-solution-page (board words)
  (let ((board (ensure-boggle-board board)))
   (cl-who:with-html-output-to-string (s nil :indent t)
     (:html
      (:head (:style (cl-who:str (css-lite:css
				   (("table#board") (:position "fixed" :top "0" :left "0"))
				   (("table#board,td#board") (:text-align "center" :font-size "xx-large"))
				   (("table#words") (:position "relative" :left 200)))))
	     (:script (cl-who:str
		       (parenscript:ps-compile-file (merge-pathnames "boggle-ps-highlighting.lisp" *boggle-root*)))))
      (:body
       (cl-who:str (boggle-board-html board))
       (cl-who:str (solutions-html (solve-boggle-board board words))))))))
(hunchentoot:define-easy-handler (run-boggle :uri "/run") (letters language)
  (let ((board (make-board-from-string (to-letters letters)))
	(words (if (and language (stringp language) (string= language "en"))
		   (read-english-words)
		   (read-dutch-words))))
    (create-solution-page board words)))

(defun extract-board-from-uri (&optional (default "simpleboardtests"))
  (let* ((uri (hunchentoot:request-uri hunchentoot:*request*))
	 (scan-result (multiple-value-list (cl-ppcre:scan "/((en)|(nl))/([a-z]+)" uri))))
    (if (car scan-result)
	(destructuring-bind (from to starts ends) scan-result
	  (declare (ignorable from to))
	  (subseq uri (elt starts 3) (elt ends 3)))
	default)))
(defun handle-en-board ()
  (create-solution-page
   (extract-board-from-uri "somedefaultlettr")
   (read-english-words)))
(defun handle-nl-board ()
  (create-solution-page
   (extract-board-from-uri "eenstandaardbord")
   (read-dutch-words)))


