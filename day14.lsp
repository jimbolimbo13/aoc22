;; Day14
;;

(ql:quickload :cl-ppcre )

(defun load-file (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil)
          while line
          collect line)))

(defvar *file* nil) ;;procedure

(setf *file* (load-file "day14_input.txt")) ;; procedure
;(setf *file* (load-file "./test_input_day14.txt")) ;; procedure

(defun parse-input (file-list)
  (let ((cave (make-array '(500 1000) :initial-element nil))
        (start-pos)
        (end-pos)
        (lowest-rock 0))
   (dolist (next file-list)
     (let ((coords (mapcar #'(lambda (x) (cl-ppcre:split "," x))
                           (cl-ppcre:split " -> " next))))
       (dotimes (ix (1- (length coords)))
         (setf start-pos (mapcar #'parse-integer (nth ix coords)))
         (setf end-pos (mapcar #'parse-integer (nth (1+ ix) coords)))
         (if (equal (first start-pos) (first end-pos))
             (progn ; same x values, so we draw the line along Y axis
               (let ((y-min (min (second start-pos) (second end-pos)))
                     (y-max (max (second start-pos) (second end-pos)))
                     (x-ix (first start-pos)))
                 (setf lowest-rock (max lowest-rock y-max))
                 (loop for y-ix from y-min to y-max do
                     (setf (aref cave y-ix x-ix) 't))))
             (progn
               (let ((x-min (min (first start-pos) (first end-pos)))
                     (x-max (max (first start-pos) (first end-pos)))
                     (y-ix (second start-pos)))
                 (setf lowest-rock (max lowest-rock y-ix))
                 (loop for x-ix from x-min to x-max do
                   (setf (aref cave y-ix x-ix) 't)))))
         )))
    (list cave lowest-rock)))

(defun sandfall-1 (cave-data)
  (let ((start '(500 0))
        (cur-pos '(500 0))
        (sand-count 0)
        (lowest-rock (second cave-data))
        (cave (first cave-data)))
  (loop
    (cond
      ((< lowest-rock (second cur-pos))
       (return))
      ((not (aref cave (1+ (second cur-pos)) (first cur-pos)))
       (setf cur-pos (list (first cur-pos) (1+ (second cur-pos)))))
      ((not (aref cave (1+ (second cur-pos)) (1- (first cur-pos))))
       (setf cur-pos (list (1- (first cur-pos)) (1+ (second cur-pos)))))
      ((not (aref cave (1+ (second cur-pos)) (1+ (first cur-pos))))
       (setf cur-pos (list (1+ (first cur-pos)) (1+ (second cur-pos)))))
      ('t
       (incf sand-count)
       (setf (aref cave (second cur-pos) (first cur-pos)) 't)
       (setf cur-pos start))))
    sand-count))

(format t "Day14 Part1: ~A~%" (sandfall-1 (parse-input *file*)))



(defun sandfall-2 (cave-data)
  (let ((start '(500 0))
        (cur-pos '(500 0))
        (sand-count 0)
        (floor (+ 2 (second cave-data)))
        (cave (first cave-data)))
    (dotimes (ix 1000) ; same size as the array I created
      (setf (aref cave floor ix) 't))
  (loop
    (cond
      ((aref cave 0 500)
       (return))
      ((not (aref cave (1+ (second cur-pos)) (first cur-pos)))
       (setf cur-pos (list (first cur-pos) (1+ (second cur-pos)))))
      ((not (aref cave (1+ (second cur-pos)) (1- (first cur-pos))))
       (setf cur-pos (list (1- (first cur-pos)) (1+ (second cur-pos)))))
      ((not (aref cave (1+ (second cur-pos)) (1+ (first cur-pos))))
       (setf cur-pos (list (1+ (first cur-pos)) (1+ (second cur-pos)))))
      ('t
       (incf sand-count)
       (setf (aref cave (second cur-pos) (first cur-pos)) 't)
       (setf cur-pos start))))
    sand-count))


(format t "Day14 Part2: ~A~%" (sandfall-2 (parse-input *file*)))
