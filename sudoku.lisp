;;;; sudoku.lisp

(in-package #:sudoku)
(use-package :alexandria)

;;;; generic stuff
(defmacro vref (array &rest subscripts)
  `(aref ,array ,@(loop for i in subscripts collect (list '1- i))))

(defun vref1 (array subscript)
  (aref array (1- subscript)))

(defmacro subvec (vector start &optional end)
  `(subseq ,vector (1- ,start) ,end))

(defun range (a b)
  (unless (> a b) (cons a (range (1+ a) b))))

(defun subvecs  (s n)
  (when (> (length s) 0)
      (cons (subvec s 1 n) (subvecs (subvec s (1+ n)) n))))

(defun search-fixpoint (func param &optional (n 1))
  (let ((result (funcall func param)))
    (if (equalp result param)
        (values result n)
        (search-fixpoint func result (1+ n)))))

(defun multiple (x list &key (key #'identity))
  (member x (cdr (member x list :key key)) :key key))

;;; specific stuff

(defparameter rows
  (labels ((rows (n)
             (if (<= n 81)
                 (cons (range n (+ n 8)) (rows (+ n 9))))))
    (rows 1)))

(defparameter columns
  (apply #'mapcar #'list rows))

(defparameter square1
  (list 1 2 3
        10 11 12
        19 20 21))

(defparameter squares
  (mapcar #'(lambda (y)
              (mapcar #'(lambda (x)
                          (+ x y))
                      square1))
          (list 0 3 6
                27 30 33
                54 57 60)))

(defparameter groups (append rows columns squares))

(defun suppresses (n groups)
  (when groups
    (if (member n (car groups))
        (union (car groups) (suppresses n (cdr groups)))
        (suppresses n (cdr groups)))))

(defparameter suppresses ;; map: in which group is field n
  (map 'vector #'(lambda (z)
                   (remove z (suppresses z groups)))
       (range 1 81)))

(defconstant _ 0)

(defparameter hard-riddle
  (vector
    _ _ _   _ 9 _   _ 5 _
    2 _ _   _ _ 8   _ _ _
    6 _ _   _ _ _   _ _ _
    
    _ _ _   3 5 _   _ _ _
    7 _ _   _ _ _   _ _ 6
    _ 9 _   _ 4 _   _ _ _
    
    _ 5 _   _ _ _   _ 4 _
    _ _ _   _ _ 7   _ 1 2
    _ _ _   _ _ 9   _ _ _
    ))

(defparameter hard-sudoku
  (map 'vector #'(lambda (x)
                   (if (= x 0)
                       (list 1 2 3 4 5 6 7 8 9)
                       (list x)))
       hard-riddle))

(assert (= 81 (length hard-riddle)))

(defparameter riddle
      (vector
    2 1 _   _ _ _   4 _ _    
    _ _ _   _ 2 8   _ _ _ 
    _ _ _   _ _ _   1 _ 6 
    
    _ _ _   5 _ 7   6 _ 8 
    8 3 _   _ _ _   _ _ 7 
    _ _ _   _ 1 6   _ _ 3 
    
    _ 4 2   3 _ _   _ _ _ 
    _ 5 3   _ _ _   _ 7 _ 
    _ _ 7   9 _ _   _ _ _
    ))

(defparameter sudoku
  (map 'vector #'(lambda (x)
                   (if (= x 0)
                       (list 1 2 3 4 5 6 7 8 9)
                       (list x)))
       riddle))

(assert (= 81 (length riddle)))

(defun print-sudoku (sudoku)
  (format t "~&~{~{~1,10T~{~A~}~}~%~}"
          (subvecs (coerce sudoku 'list) 9)))

(defun calculate-forbidden(sud)
  (let ((sudoku (map 'vector #'copy-list sud)))
    (do ((field 1 (1+ field)))
        ((> field 81))
      (let ((allowed-numbers (vref sudoku field)))
        (if (null (cdr allowed-numbers))
            (let ((solved-number (car allowed-numbers)))
              (format t "~&deleting ~A from fields ~{~A ~}" solved-number (vref suppresses field))
              (mapc #'(lambda (n)
                        (setf (vref sudoku n) (remove solved-number (vref sudoku n)))
                        (unless (vref sudoku n) (progn (print 'OOPS) (return-from calculate-forbidden))))
                    (vref suppresses field))
              ))))
    (print-sudoku sudoku)
    sudoku))

(defun calculate-forced (sud)
  (let ((sudoku (map 'vector #'copy-list sud)))
    (dolist (group groups)
      (print group)
      (let ((together (loop for field in group append (vref sudoku field))))
        (print together)
        (loop for number from 1 to 9 do
             (unless  (multiple number together)
;               (format t "~&number ~A may be forced" number)
               (loop for place in group do
                    (let ((possible-numbers (vref sudoku place)))
                      (when (and (cdr possible-numbers)
                                 (member number possible-numbers))
                        (setf (vref sudoku place) (list number))
                        (format t "~&~A is forced for field ~A" number place))))))))
    sudoku))

(print (search-fixpoint
	(compose #'calculate-forced #'calculate-forbidden)
	sudoku))





	   
