;; Day8
;;
;;
;;

(ql:quickload :cl-ppcre )

(defun load-file (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil)
          while line
          collect line)))

(defvar *file* nil) ;;procedure

(setf *file* (load-file "day8_input.txt")) ;; procedure
;(setf *file* (load-file "test_input_day8.txt")) ;; procedure

(defun get-x-y (x y string-list)
  (- (char-int (elt (elt string-list y) x)) 48))

(defun find-hidden (file-list)
  (let* (
        (min-height -1)
        (maxY (length file-list))
        (maxX (length (first file-list)))
        (tracker (make-array (list maxY maxX)
                             :element-type 'int
                             :initial-element 0))
        (from-top-max -1)
        (from-bottom-max -1)
        (from-left-max -1)
        (from-right-max -1))
    ;; with lisp, I can loop through an arbitrary number of values simultaneously...
    ;; inner loop is the direction "from" I care about
    ;; this only works because it's a square
    (do (
         ;; (from-top-x 0 (1+ from-top-y))
         ;; (from-bottom-x 0 (1+ from-bottom-y))
         ;; (from-right-y maxX (1- from-right-y))
         ;; (from-left-y 0 (1+ from-left-y))
         (outer-loop 0 (1+ outer-loop)))
        ((<= maxY outer-loop))
      (do ((from-top-y 0 (1+ from-top-y))
           (from-bottom-y (1- maxX) (1- from-bottom-y))
           (from-left-x 0 (1+ from-left-x))a
           (from-right-x (1- maxX) (1- from-right-x)))
          ((<= maxY from-top-y))
        (format t "coord: ~d,~d~%" outer-loop from-top-y)
        (format t "~d~%~d~%~d~%" from-bottom-y from-left-x from-right-x)
        (when (< from-top-max (get-x-y outer-loop from-top-y file-list))
          (setf (aref tracker from-top-y outer-loop) 1)
          (setf from-top-max (get-x-y outer-loop from-top-y file-list))) ; from-top, outer loop is x
        (when (< from-bottom-max (get-x-y outer-loop from-bottom-y file-list))
          (setf (aref tracker from-bottom-y outer-loop ) 1)
          (setf from-bottom-max (get-x-y outer-loop from-bottom-y file-list)))
        (when (< from-left-max (get-x-y from-left-x outer-loop file-list))
          (setf (aref tracker outer-loop from-left-x) 1)
          (setf from-left-max (get-x-y from-left-x outer-loop file-list)))
        (when (< from-right-max (get-x-y from-right-x outer-loop file-list))
          (setf (aref tracker outer-loop from-right-x) 1)
          (setf from-right-max (get-x-y from-right-x outer-loop file-list))))
      (setf from-top-max -1)
      (setf from-bottom-max -1)
      (setf from-left-max -1)
      (setf from-right-max -1))
    (let ((total-seen 0))
        (dotimes (y maxY)
          (dotimes (x maxX)
            (setf total-seen (+ total-seen (aref tracker y x)))))
      total-seen)
    ;; (count-if-not #'zerop
    ;;               tracker
    ;;               :key #'(lambda (x) (count-if-not #'zerop x)))
    ;; ; apparently an array is not a sequence *shrug*
    ))


(format t "Day8 part1: ~d~%" (find-hidden *file*))
