(in-package :boggle)

(defun start-html-server ()
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4242)))

(defun to-letters (letters)
  (if (and letters (stringp letters) (= (length letters) 16))
      letters
      (random-boggle-string)))

(defun boggle-board-html (board)
  (cl-who:with-html-output-to-string (s)
    (:table :border 2 :cellpadding 5
	    (loop for row from 0 to 3 do
		 (cl-who:htm (:tr
			      (loop for column from 0 to 3 do
				   (cl-who:htm (:td (cl-who:str (letter board (make-position row column))))))))))))

(hunchentoot:define-easy-handler (run-boggle :uri "/run") (letters language)
  (let ((board (make-board-from-string (to-letters letters)))
	(words (if (and language (stringp language) (string= language "en"))
							     (read-english-words)
							     (read-dutch-words))))
    (format nil "~a~%~a" (boggle-board-html board) (solve-boggle-board board words))))
