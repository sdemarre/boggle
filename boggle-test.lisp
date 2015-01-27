(in-package :boggle)

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
    (verify (list-equal (possible-word-range words "a") '(0 6)))
    (verify (list-equal (possible-word-range words "ab") '(nil nil)))
    (verify (list-equal (possible-word-range words "sg") '(78 78)))
    (verify (list-equal (possible-word-range words "wo") '(92 93)))
    (verify (list-equal (possible-word-range words "de") '(nil nil)))
    (verify (list-equal (possible-word-range words "o") '(56 62)))
    (verify (list-equal (possible-word-range words "oa") '(56 57)))
    (verify (list-equal (possible-word-range words "f") '(nil nil)))
    (verify (list-equal (possible-word-range words "oaa") '(56 56)))
    (verify (list-equal (possible-word-range words "oaf") '(57 57)))
    (verify (list-equal (possible-word-range words "z") '(nil nil)))))
