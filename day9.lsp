;; Day9
;;
;; Start at 0,0
;; Head moves
;; Tail checks head
;;  Tail moves if needed
;; Add tail position to the position set
;; (adjoin '("A" "C") '(("A" "B")) :test #'equal)
;; repeat
;;
;;
;; pos is always X,Y


(ql:quickload :cl-ppcre )

(defun load-file (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil)
          while line
          collect line)))

(defvar *file* nil) ;;procedure

(setf *file* (load-file "day9_input.txt")) ;; procedure
;(setf *file* (load-file "./test_input_day9.txt")) ;; procedure

(defun move-dir (in-pos in-dir)
  ; return the position if there is no match on direction
  (let ((pos in-pos)
        (dir in-dir))
    (when (< 1 (length in-dir))
      ;(format t "Recursively moving tail: ~a~%" in-dir)
        (setf pos (move-dir in-pos (subseq in-dir 1)))
        (setf dir (subseq in-dir 0 1)))
    (cond
      ((equal "R" dir) ; X+1, Y
       (list (incf (first pos)) (second pos)))
      ((equal "L" dir) ; X-1, Y
       (list (decf (first pos)) (second pos)))
      ((equal "D" dir) ; X,Y-1
       (list (first pos) (decf (second pos))))
      ((equal "U" dir) ; X,Y+1
       (list (first pos) (incf (second pos))))
      ('t
       pos))))

(defun check-head (head tail)
  ; check postion of head if tail needs to move
  ; returns nil or direction to move
  (let ((x-diff (- (first head) (first tail)))
        (y-diff (- (second head) (second tail))))
    (cond
      ((and (>= 1 (abs x-diff)) (>= 1 (abs y-diff)))
       (return-from check-head nil))
      ((and (< 0 (abs x-diff)) (< 0 (abs y-diff))) ; diagonal move
       (if (< 0 x-diff) ; X is greater, needs R
           (if (< 0 y-diff)
               "RU" ; Y is greater, needs U
               "RD") ; Y is less, needs D
           (if (< 0 y-diff) ; X is smaller, needs L
               "LU"
               "LD")))
      ((< 1 (abs x-diff)) ; higher X value, move right or left
       (if (< 0 x-diff)
           "R" ; H is more positive, so move right
           "L"))
      ((< 1 (abs y-diff)) ; higher Y value, move up or down
       (if (< 0 y-diff)
           "U"
           "D")))))

(defun record-pos (pos tail-trail)
  (if (member pos tail-trail :test #'equal)
      tail-trail
      (append tail-trail (list pos ))))

(defun count-tails (file-list)
 (let ((head '(0 0))
       (tail '(0 0))
       (tail-trail '()))
   (setf tail-trail (list '(0 0)))
   (dolist (next file-list)
     (let ((next-move (cl-ppcre:split :whitespace-char-class next)))
       (dotimes (mv (parse-integer (second next-move)))
         (setf head (move-dir head (first next-move)))
         (setf tail (move-dir tail (check-head head tail)))
         (setf tail-trail (record-pos (copy-list tail) tail-trail))
         )))
   (length tail-trail)))

(format t "Day9 Part1: ~d~%" (count-tails *file*))


(defun count-9-tails (file-list)
 (let ((head '(0 0))
       (t1 '(0 0))
       (t2 '(0 0))
       (t3 '(0 0))
       (t4 '(0 0))
       (t5 '(0 0))
       (t6 '(0 0))
       (t7 '(0 0))
       (t8 '(0 0))
       (tail '(0 0))
       (tail-trail '()))
   (setf tail-trail (list '(0 0)))
   (dolist (next file-list)
     (let ((next-move (cl-ppcre:split :whitespace-char-class next)))
       (dotimes (mv (parse-integer (second next-move)))
         (setf head (move-dir head (first next-move)))
         (setf t1 (move-dir t1 (check-head head t1)))
         (setf t2 (move-dir t2 (check-head t1 t2)))
         (setf t3 (move-dir t3 (check-head t2 t3)))
         (setf t4 (move-dir t4 (check-head t3 t4)))
         (setf t5 (move-dir t5 (check-head t4 t5)))
         (setf t6 (move-dir t6 (check-head t5 t6)))
         (setf t7 (move-dir t7 (check-head t6 t7)))
         (setf t8 (move-dir t8 (check-head t7 t8)))
         (setf tail (move-dir tail (check-head t8 tail)))
         (setf tail-trail (record-pos (copy-list tail) tail-trail))
         )))
   (length tail-trail)))


(format t "Day9 Part2: ~d~%" (count-9-tails *file*))
