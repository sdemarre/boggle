;;;; boggle.lisp

(in-package #:boggle)

(defun make-position (row column)
  (cons row column))
(defun position-row (position)
  (car position))
(defun position-column (position)
  (cdr position))

(defclass boggle-board ()
  ((letters :accessor letters :initform (make-array (list +board-num-rows+ +board-num-columns+)))))

(defmethod letter ((self boggle-board) position)
  (aref (letters self) (position-row position) (position-column position)))

(defsetf letter (boggle-board position) (new-letter)
  `(setf (aref (letters ,boggle-board) (position-row ,position) (position-column ,position)) ,new-letter))


(defmethod print-object (boggle-board stream)
  (print-unreadable-object (boggle-board stream)
			   (do-on-rows row
			     (format stream "~%[")
			     (do-on-columns column
			       (format stream "~a~a" (letter boggle-board (make-position row column))
				       (if (= column (1- +board-num-columns+))
					   ""
					   " ")))
			     (format stream "]"))
			   (format stream "~%")))

(defun random-letter ()
  (code-char (+ (char-code #\a) (random (1+ (- (char-code #\z) (char-code #\a)))))))
(let ((letter-cumul-freqs '(817 966 1244 1669 2940 3162 3364 3973 4670 4685 4762 5165 5406 6080 6831 7024
			    7034 7632 8265 9171 9446 9544 9780 9795 9993 10000)))
  (defun random-english-letter ()
    (let ((r (random 10000)))
      (code-char (+ (char-code #\a) (position-if #'(lambda (n) (< r n)) letter-cumul-freqs))))))

(defun random-cube-letter (cube)
  (elt cube (random 6)))
(defun random-boggle-string ()
  (let* ((cubes '("aaeegn" "abbjoo" "achops" "affkps"
		  "aoottw" "cimotu" "deilrx" "delrvy"
		  "distty" "eeghnw" "eeinsu" "ehrtvw"
		  "eiosst" "elrtty" "himnqu" "hlnnrz"))
	 (random-cubes (mapcar 'random-cube-letter cubes)))
    (coerce (alexandria:shuffle random-cubes)
	    'string)))
(defun random-boggle-board ()
  (ensure-boggle-board (random-boggle-string)))


(defun fill-board-randomly (boggle-board)
  (do-on-positions position
    (setf (letter boggle-board position) (random-english-letter)))
  boggle-board)

(defun fill-board (boggle-board letters)
  (let ((index -1))
    (do-on-positions position
      (setf (letter boggle-board position) (elt letters (incf index)))))
  boggle-board)

(defun make-board-from-string (letters)
  (fill-board (make-instance 'boggle-board) letters))

(defun board-letter-string (boggle-board)
  (coerce
   (iter outer (for r from 0 to (1- +board-num-rows+))
	 (iter (for c from 0 to (1- +board-num-columns+))
	       (in outer (collect (letter boggle-board (make-position r c))))))
   'string))

(defun valid-board-position (position)
  (with-position (row column position)
      (and (>= row 0)
	   (>= column 0)
	   (< row +board-num-rows+)
	   (< column +board-num-columns+))))
(defun possible-extensions (current-position)
  (with-position (current-row current-column current-position)
    (if (not (valid-board-position (make-position current-row current-column)))
	(iter outer (for row from 0 to (1- +board-num-rows+))
	      (iter (for column from 0 to (1- +board-num-columns+))
		    (in outer (collect (cons row column)))))
	(iter outer (for row from (1- current-row) to (1+ current-row))
	      (iter (for column from (1- current-column) to (1+ current-column))
		    (when (or (not (= row current-row)) (not (= column current-column)))
		      (when (valid-board-position (make-position row column))
			(in outer (collect (make-position row column))))))))))


(defun read-words (filename)
  (let ((raw-words (iter (for line in-file filename using #'read-line)
			 (collect (cl-ppcre:regex-replace "" line "")))))
    (coerce
     (remove-if #'(lambda (l) (or (< l 3) (< 16 l))) raw-words :key #'length)
     'vector)))
(defparameter *boggle-root*
  #+win32 "c:/users/serge.demarre/appdata/roaming/src/lisp/systems/boggle/"
  #+unix "/home/serge/src/lisp/my-projects/boggle/"
)
(defun read-english-words ()
  (let ((filename (merge-pathnames "wordsEn.txt" *boggle-root*)))
    (read-words filename)))

(defun read-dutch-words ()
  (let ((filename (merge-pathnames "wordsDu.txt" *boggle-root*)))
    (read-words filename)))

(defun dict-word (words word-index)
  (elt words word-index))
(defun dict-word-has-enough-letters-p (words word-index letter-index)
  (word-has-enough-letters-p (dict-word words word-index) letter-index))
(defun dict-word-letter (words word-index letter-index)
  (elt (elt words word-index) letter-index))
(defun word-has-enough-letters-p (word letter-index)
  (> (length word) letter-index))
(defun word-with-bigger-letter-p (word letter-index letter)
  (and (word-has-enough-letters-p word letter-index)
       (char>= (elt word letter-index) letter)))
(defun word-with-letter-p (word letter-index letter)
  (and (word-has-enough-letters-p word letter-index)
       (char= (elt word letter-index) letter)))

(defun binary-search (sequence compare range-low range-high)
  (let* ((first range-low)
	 (last range-high)
	 (count (- last first)))
    (iter (while (> count 0))
	  (let* ((count2 (floor count 2))
		 (mid (+ first count2)))
	    (if (funcall compare (elt sequence mid))
		(progn
		  (setf first (incf mid))
		  (decf count (1+ count2)))
		(setf count count2))))
    first))
(defun first-word-with-letter-fast (words range-low range-high letter-index letter)
  (binary-search words #'(lambda (word) (and (< letter-index (length word))
					     (char< (elt word letter-index) letter)))
		 range-low range-high))
(defun last-word-with-letter-fast (words range-low range-high letter-index letter)
  ;; precondition (char= letter (elt (elt words range-low) letter-index))
  (unless (char= letter (elt (elt words range-low) letter-index))
    (error "Precondition not met"))
  (binary-search words #'(lambda (word) (and (< letter-index (length word))
					     (char< (elt word letter-index) (code-char (1+ (char-code letter))))))
		 range-low range-high))
(defun possible-letter-range-fast (words range-low range-high letter-index letter)
  (let ((first (first-word-with-letter-fast words range-low range-high letter-index letter)))
    (if (and (< first (length words))
	     (< letter-index (length (elt words first)))
	     (char= letter (elt (elt words first) letter-index)))
	(let ((last (last-word-with-letter-fast words first range-high letter-index letter)))
	  (if (= first last)
	      (list first first)
	      (list first (1- last))))
	(list nil nil))))
(defun possible-word-range (words word)
  (let ((current-range-low 0)
	(current-range-high (1- (length words))))
    (iter (for letter-index from 0 to (1- (length word)))
	  (while (and (not (null current-range-low))
		      (not (null current-range-high))))
	  (destructuring-bind (new-range-low new-range-high)
	      (possible-letter-range-fast words current-range-low current-range-high
					  letter-index (elt word letter-index))
	    (setf current-range-low new-range-low
		  current-range-high new-range-high)))
    (list current-range-low current-range-high)))

(defun word-list (words range)
  (iter (for i from (car range) to (cadr range))
	(collect (dict-word words i))))

(defun possible-word-list (words word)
  (destructuring-bind (range-start range-end)
      (possible-word-range words word)
    (when (and range-start range-end)
     (word-list words (list range-start range-end)))))

(defun position-equal (pos1 pos2)
  (with-position (r1 c1 pos1)
    (with-position (r2 c2 pos2)
      (and (= r1 r2) (= c1 c2)))))
(defun path-has-position (path position)
  (and (not (null path))
       (or (position-equal (car path) position)
	   (path-has-position (cdr path) position))))

(defun format-path (path)
  (format nil "~{~a~^, ~}"
	  (iter (for position in (cdr (reverse path)))
		(collect (with-position (r c position)
			   (format nil "[~a, ~a]" r c))))))
(defun empty-range-p (range)
  (and (null (car range))
       (null (cadr range))))

(defun compute-solutions (boggle-board words
			  current-words current-word current-path
			  &optional collect-solution)
  (iter (for path-extension in (possible-extensions (car current-path)))
	(when (not (path-has-position current-path path-extension))
	  (let* ((new-path (cons path-extension current-path))
		 (new-word (format nil "~a~a" current-word (letter boggle-board path-extension)))
		 (word-range (possible-word-range words new-word)))
	    (unless (empty-range-p word-range)
	      (when (>= (length new-word) 3)
		(when (string= new-word (dict-word words (car word-range)))
                  (if collect-solution (funcall collect-solution new-word new-path)
                      (format t "~a, ~a~%" new-word (format-path new-path)))))
	      (compute-solutions boggle-board words current-words new-word new-path collect-solution))))))
(defun ensure-boggle-board (board-or-string)
  (if (stringp board-or-string)
      (fill-board (make-instance 'boggle-board) board-or-string)
      board-or-string))
(defun solve-boggle-board (boggle-board-or-string words &optional (initial-row -1) (initial-column -1))
  (let (result
	(boggle-board (ensure-boggle-board boggle-board-or-string)))
    (compute-solutions boggle-board words nil "" (list (make-position initial-row initial-column))
		       #'(lambda (new-word new-path) (push (list new-word (rest (reverse new-path))) result)))
    (reverse (remove-duplicates result :test #'string= :key #'car))))

(defun find-best-board (words order-fun &optional (number-tries 10000))
  (iter (for i from 1 to number-tries)
	(let ((board (random-boggle-board)))
	  (let ((solutions (solve-boggle-board board words)))
	    (finding board maximizing (funcall order-fun solutions))))))

(defun count-words-longer-than (solutions length)
  (iter (for solution in solutions)
	(when (>= (length (car solution)) length)
	  (summing 1))))
(defun compute-word-length-value (solutions)
  (iter (for solution in solutions)
	(summing (expt 4 (length (car solution))))))
(defun max-word-length (solutions)
  (reduce #'max solutions :key #'(lambda (w) (length (cadr w)))))
(defun find-board-with-longest-words (words &optional (number-tries 10000))
  (iter (for i from 1 to number-tries)
	(let ((board (fill-board-randomly (make-instance 'boggle-board))))
	  (let ((solutions (solve-boggle-board board words)))
	    (finding (list board (length solutions) (compute-word-length-value solutions) (max-word-length solutions))
		     maximizing (compute-word-length-value solutions))))))

(defun find-best-board-parallel (words order-fun &optional (tries-per-thread 1000) (num-threads 4))
  (let ((threads (iter (for i from 1 to num-threads)
		       (collect (sb-thread:make-thread #'(lambda () (find-best-board words order-fun tries-per-thread)))))))
    (iter (for board in (mapcar #'sb-thread:join-thread threads))
	  (finding board maximizing (funcall order-fun (solve-boggle-board board words))))))
