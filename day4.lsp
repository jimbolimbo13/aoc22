;; Day 4

(defun load-file (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil)
          while line
          collect line)))

(defvar *file* nil) ;;procedure

(setf *file* (load-file "day4_input.txt")) ;; procedure
;(setf *file* (load-file "./test_input_day4.txt")) ;; procedure


(defun split-line (full-line)
  (let* (
         (midpoint (position #\, full-line))
         (left-dash (position #\- full-line))
         (right-dash (+ midpoint (position #\- (subseq full-line midpoint))))
          )
  (list
    (list
    (parse-integer (subseq full-line 0 left-dash))
    (parse-integer (subseq full-line (+ left-dash 1) midpoint)))
    (list
    (parse-integer (subseq full-line (+ midpoint 1) right-dash))
    (parse-integer (subseq full-line (+ right-dash 1)))))))

(defun find-overlap (assignments)
    (or
        (and
        (>= (caar assignments) (caadr assignments))
        (<= (cadar assignments) (cadadr assignments)))
        (and
        (>= (caadr assignments) (caar assignments))
        (<= (cadadr assignments) (cadar assignments)))))

(defun count-overlaps (assignment-list)
  (let ((running-count 0))
    (dolist (assignment-pair assignment-list)
      (if (find-overlap (split-line assignment-pair))
          (setq running-count (+ running-count 1))))
    running-count))

(format t "Day4, Part 1 answer: ~d~%" (count-overlaps *file*)) ;;procedure
