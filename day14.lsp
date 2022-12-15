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

(defvar *min-x* 500)
(defvar *max-x* 500)
(defvar *min-y* 0)
(defvar *cave-rocks* (make-array '(500 1000) :initial-element nil))

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
                 (setf *min-x* (min *min-x* x-ix))
                 (setf *max-x* (max *max-x* x-ix))
                 (loop for y-ix from y-min to y-max do
                   (setf (aref *cave-rocks* y-ix x-ix) 't)
                   (setf (aref cave y-ix x-ix) 't))))
             (progn
               (let ((x-min (min (first start-pos) (first end-pos)))
                     (x-max (max (first start-pos) (first end-pos)))
                     (y-ix (second start-pos)))
                 (setf lowest-rock (max lowest-rock y-ix))
                 (setf *min-x* (min *min-x* x-min))
                 (setf *max-x* (max *max-x* x-max))
                 (loop for x-ix from x-min to x-max do
                   (setf (aref *cave-rocks* y-ix x-ix) 't)
                   (setf (aref cave y-ix x-ix) 't)))))
         )))
    (setf *min-y* lowest-rock)
    (list cave lowest-rock)))

(defun sandfall-1 (cave-data &optional pretty-print verbose-print)
  (let ((start '(500 0))
        (cur-pos '(500 0))
        (sand-count 0)
        (lowest-rock (second cave-data))
        (cave (first cave-data)))
  (loop
    (when verbose-print (print-cave cave cur-pos))
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
       (when pretty-print (print-cave cave cur-pos))
       (setf cur-pos start))))
    (when (or verbose-print pretty-print) (print-cave cave cur-pos))
    sand-count))

(defun print-cave (cave cur-pos)
  (format t "~%~%~%~%~%~%~%~%~%~%~%~%~%~%")
  (dotimes (y-ix (1+ *min-y*))
    (dotimes (x-raw (- *max-x* *min-x*))
      (let ((x-ix (+ x-raw *min-x*)))
        (cond
          ;; ((and (aref *cave-rocks* y-ix x-ix)
          ;;       (not (aref cave y-ix x-ix)))
          ((aref *cave-rocks* y-ix x-ix)
           (format t "#"))
          ((and (equal x-ix (first cur-pos))
                (equal y-ix (second cur-pos)))
           (format t "+"))
          ((and (not (aref *cave-rocks* y-ix x-ix))
                (not (aref cave y-ix x-ix)))
           (format t "."))
          ((aref cave y-ix x-ix)
           (format t "o")))))
    (format t "~%"))
  (sleep 0.05))




(defun sandfall-2 (cave-data &optional pretty-print verbose-print)
  (let ((start '(500 0))
        (cur-pos '(500 0))
        (sand-count 0)
        (floor (+ 2 (second cave-data)))
        (cave (first cave-data)))
    (dotimes (ix 1000) ; same size as the array I created
      (setf (aref cave floor ix) 't))
  (loop
    (when verbose-print (print-cave cave cur-pos))
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
       (when pretty-print (print-cave cave cur-pos))
       (setf cur-pos start))))
    (when (or verbose-print pretty-print) (print-cave cave cur-pos))
    sand-count))

; (setf part1 (sandfall-1 (parse-input *file*) 't 't)) ; to watch the sand fall

(setf part1 (sandfall-1 (parse-input *file*)))
(setf part2 (sandfall-2 (parse-input *file*)))

(format t "Day14 Part1: ~A~%" part1)
(format t "Day14 Part2: ~A~%" part2)
