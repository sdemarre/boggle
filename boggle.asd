;;;; boggle.asd

(asdf:defsystem #:boggle
  :description "Describe boggle here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (:iterate :alexandria)
  :components ((:file "package")
	       (:file "boggle-macros")
               (:file "boggle")))

