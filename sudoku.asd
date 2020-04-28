;;;; sudoku.asd

(asdf:defsystem #:sudoku
  :description "Describe sudoku here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria #:uiop)
  :components ((:file "package")
               (:file "sudoku")))
