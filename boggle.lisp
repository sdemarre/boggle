;;;; boggle.lisp

(in-package #:boggle)

;;; "boggle" goes here. Hacks and glory await!
(defconstant +board-num-rows+ 4)
(defconstant +board-num-columns+ 4)


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

(defmacro do-on-letters (row-var column-var &body body)
  `(do-on-rows ,row-var
     (do-on-columns ,column-var
       ,@body)))

(defmacro do-on-rows (row-var &body body)
  `(iter (for ,row-var from 0 to (1- +board-num-rows+))
	 ,@body))

(defmacro do-on-columns (column-var &body body)
  `(iter (for ,column-var from 0 to (1- +board-num-columns+))
	 ,@body))

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

(defun fill-board-randomly (boggle-board)
  (do-on-letters row column
    (setf (letter boggle-board (make-position row column)) (random-english-letter)))
  boggle-board)

(defmacro with-position ((row column position) &body body)
  `(let ((,row (position-row ,position))
	 (,column (position-column ,position)))
     ,@body))

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


(defun read-english-words ()
  (let (
#+win32(filename "c:/users/serge.demarre/appdata/roaming/src/lisp/systems/boggle/wordsEn.txt")
       #+unix(filename "/home/serge/src/lisp/my-systems/boggle/wordsEn.txt")
       )
   (coerce (iter (for line in-file filename using #'read-line)
		 (collect (subseq line 0 (1- (length line)))))
	   'vector)))
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

(defparameter *1-letter-ranges* nil)
(defun first-word-with-letter (words letter)
  (position-if #'(lambda (word) (char= letter (elt word 0))) words))
(defun last-word-with-letter (words first-word letter)
  (when first-word
    (let ((p (position-if-not #'(lambda (word) (char= letter (elt word 0))) words :start first-word)))
      (if p (1- p)
	  (1- (length words))))))
(defun 1-letter-ranges (words)
  (let ((result (make-array 26)))
    (iter (for letter from (char-code #\a) to (char-code #\z))
          (for index from 0)
          (let ((first-word (first-word-with-letter words (code-char letter))))
	    (setf (elt result index)
		  (list first-word (last-word-with-letter words first-word (code-char letter))))))
    result))
(defmacro with-1-letter-ranges (words &body body)
  `(let ((*1-letter-ranges* (1-letter-ranges ,words)))
     ,@body))
(defun possible-letter-range (words current-range-low current-range-high letter-index letter)
  (if (and (zerop letter-index) (not (null *1-letter-ranges*)))
      (elt *1-letter-ranges* (- (char-code letter) (char-code #\a)))
      (cond ((or (and (dict-word-has-enough-letters-p words current-range-low letter-index)
                      (char< letter (dict-word-letter words current-range-low letter-index)))
                 (and (dict-word-has-enough-letters-p words current-range-high letter-index)
                      (char> letter (dict-word-letter words current-range-high letter-index))))
             (list nil nil))
            (t
             (let* ((new-range-low
                     (position-if #'(lambda (word) (word-with-letter-p word letter-index letter))
                                  words
                                  :start current-range-low :end (1+ current-range-high)))
                    (new-range-high
                     (when new-range-low
                       (position-if #'(lambda (word) (word-with-letter-p word letter-index letter))
                                    words
                                    :start new-range-low :end (1+ current-range-high) :from-end t))))
               (list new-range-low (if new-range-low (if new-range-high
                                                         new-range-high
                                                         current-range-high)
                                       nil)))))))
(defun possible-word-range (words word)
  (let ((current-range-low 0)
	(current-range-high (1- (length words))))
    (iter (for letter-index from 0 to (1- (length word)))
	  (while (and (not (null current-range-low))
		      (not (null current-range-high))))
	  (destructuring-bind (new-range-low new-range-high)
	      (possible-letter-range words current-range-low current-range-high
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

(defun list-equal (l1 l2)
  (if (and (not (null l1))
	   (not (null l2)))
      (or (and (null (car l1))
	       (null (car l2))
	       (list-equal (cdr l1) (cdr l2)))
	  (and (numberp (car l1))
	       (numberp (car l2))
	       (= (car l1) (car l2))
	       (list-equal (cdr l1) (cdr l2))))
      (and (null l1) (null l2))))
(defmacro verify (expression)
  `(let ((result ,expression))
     (if (not result)
	 (format t "FAILED: ~a~%" ',expression))))
(defun run-tests ()
  (let ((words #("a" "aasssv" "aatta" "aenrr" "aioewn" "aisa" "ameoofn" "brye" "cb" "chahht"
		 "cn" "csdiph" "da" "dr" "dtrusoo" "eemrnl" "efoso" "efr" "eg" "egile" "eheut"
		 "ehpenna" "eii" "elesrrr" "enhrf" "ensa" "eo" "eso" "et" "ewott" "h" "hloheea"
		 "hmehah" "hmetna" "hnen" "hnsip" "hrelnr" "hu" "iel" "ineka" "iteoe" "k"
		 "kiwlei" "leati" "lnacr" "ltatct" "lwr" "mad" "mepig" "mhovo" "mna" "mrsehtt"
		 "nic" "nt" "ntch" "nthft" "oaa" "oafh" "oenmas" "osn" "ou" "oucs" "owu" "pia"
		 "piaha" "prh" "qtcte" "r" "rd" "rei" "resur" "rhoyeoi" "rn" "rntro" "rrr"
		 "rsseah" "rt" "scnrehe" "sg" "stboefl" "swocaee" "tay" "tgeah" "tiooa"
		 "trevud" "ttsre" "tuinpp" "wai" "wktrsra" "wleen" "wnaoa" "wnimds" "wol"
		 "wost" "wtenuev" "yoor" "ytrehee")))
    (with-1-letter-ranges words
      (verify (list-equal (possible-word-range words "a") '(0 6)))
      (verify (list-equal (possible-word-range words "sg") '(78 78)))
      (verify (list-equal (possible-word-range words "wo") '(92 93)))
      (verify (list-equal (possible-word-range words "de") '(nil nil)))
      (verify (list-equal (possible-word-range words "o") '(56 62)))
      (verify (list-equal (possible-word-range words "oa") '(56 57)))
      (verify (list-equal (possible-word-range words "oaa") '(56 56)))
      (verify (list-equal (possible-word-range words "oaf") '(57 57)))
      (verify (list-equal (possible-word-range words "z") '(nil nil))))))
