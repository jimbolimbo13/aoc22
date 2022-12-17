;; Day15
;;


(ql:quickload :cl-ppcre )

(defun load-file (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil)
          while line
          collect line)))

(defvar *file* nil) ;;procedure

(setf *file* (load-file "day15_input.txt")) ;; procedure
;(setf *file* (load-file "test_input_day15.txt")) ;; procedure


(defun get-distance (here there)
  (+ (abs (- (second here) (second there)))
     (abs (- (first here) (first there)))))

;; this is basically just part 2 right now

(defun parse-input (file-list)
  (let ((sensors))
    (dolist (next file-list)
      (let* ((n-split (cl-ppcre:split ",*:* " next))
             (sensor (mapcar #'(lambda (x) (parse-integer (subseq x 2)))
                             (subseq n-split 2 4)))
             (beacon (mapcar #'(lambda (x) (parse-integer (subseq x 2)))
                            (subseq n-split 8)))
             (range (get-distance sensor beacon)))
        (setf sensors (cons (list sensor beacon range) sensors))
        ))
    sensors))

(defun compare-ranges (left right)
  (cond
    ((< (first left) (first right))
     't)
    ((> (first left) (first right))
     nil)
    ((< (second left) (second right))
     't)
    ((> (second left) (second right))
     nil)
    ('t 't)))

(defun check-target-row (sensors target limit)
  (let ((target-row)
        (timer 0)
        (range-min)
        (range-max))
    (dolist (sensor-list sensors)
      (let ((sensor (first sensor-list))
            ;(beacon (second sensor-list))
            (range (third sensor-list))
            (x-diff)
            (x-min)
            (x-max))
        (setf x-diff (- range (get-distance sensor (list (first sensor) target))))
        (when (< 0 x-diff)
          (setf x-min (- (first sensor) x-diff))
          (setf x-max (+ (first sensor) x-diff))
          (when (and (<= 0 x-max) (>= limit x-min))
            (setf target-row (cons (list
                                    (max 0 x-min)
                                    (min limit x-max)) target-row)))
          )
        ))
    (setf target-row (sort target-row #'compare-ranges))
    (setf range-min 0)
    (setf range-max (second (first target-row)))
    (loop for ix from 1 to (1- (length target-row)) do
      (if (and (>= range-max (first (nth ix target-row)))
               't)
          (setf range-max (max range-max (second (nth ix target-row))))
          (if (equal (1+ range-max) (first (nth (1+ ix) target-row)))
              (progn
                (setf range-min (first (nth (1+ ix) target-row)))
                (setf range-max (first (nth (1+ ix) target-row))))
              (return-from check-target-row (1+ range-max))))
      (when (equal limit range-max)
        (return-from check-target-row nil)))
    ))

(defun find-gap (sensors limit)
  (let ((cur-x)
        (cur-y))
    (loop for ix from 0 to limit do
          (setf cur-y ix)
          (setf cur-x
                (check-target-row sensors ix limit))
          (when cur-x (return))
          (when (equal 0 (mod ix 100000))
            (format t "finished round: ~A~%" ix))
          )
    (format t "found at x, y: ~A, ~A~%" cur-x cur-y)
    (+ cur-y (* cur-x 4000000))
    ))


(format t "Day15 part2: ~A~%" (find-gap (parse-input *file*) 4000000))
