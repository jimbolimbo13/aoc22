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
  (when (or
         (or (> 0 x)
             (> 0 y))
         (or (<= (length (first string-list)) x)
             (<= (length string-list) y)))
      (return-from get-x-y -1))
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

(defconstant up '(0 -1))
(defconstant down '(0 1))
(defconstant  left '(-1 0))
(defconstant  right '(1 0))

(defun score-direction (pos pos-shift file-list)
  (let ((height (get-x-y (first pos) (second pos) file-list)))
  (do ((new-pos (mapcar #'+ pos-shift pos)
                )
       (new-height (get-x-y
                    (+ (first pos-shift) (first pos))
                    (+ (second pos-shift) (second pos)) file-list)
                     (get-x-y (first new-pos) (second new-pos) file-list))
       (score-count 0 (1+ score-count)))
      ((> 0 new-height)
       score-count)
    (setf new-pos (mapcar #'+ pos-shift new-pos))
    (when (<= height new-height)
      (return (1+ score-count)))
    )))

(defun how-scenic (file-list)
  (let ((running-count 1)
        (max-score 0)
        (cardinal (list up down left right)))
    (dotimes (y-val (length file-list))
      (dotimes (x-val (length (first file-list)))
        (dolist (next cardinal)
          (setf running-count (* running-count
                                (score-direction
                                 (list x-val y-val)
                                 next file-list)))
          )
        (when (< max-score running-count)
          (setf max-score running-count))
        (setf running-count 1)))
    max-score))

(format t "Day8 part2: ~a~%" (how-scenic *file*))
