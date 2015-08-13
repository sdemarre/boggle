;; this is parenscript, not lisp
;; solves a boggle board


(defun compute-solutions (board words current-words current-word current-path)
  (loop for path-extension in (possible-extensions ) (when (not (path-has-position current-path)))))
(defun solve-boggle-board (board words)
  (let ((result (array)))
    (compute-solutions board words (array) ""
		       (array (array 0 1))
		       (lambda (word path) (chain result (push (array word path)))))))
