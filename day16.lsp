;; Day16
;;


(ql:quickload :cl-ppcre )

(defun load-file (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil)
          while line
          collect line)))

(defvar *file* nil) ;;procedure

(setf *file* (load-file "day16_input.txt")) ;; procedure
;(setf *file* (load-file "test_input_day16.txt")) ;; procedure
;; start at my position. If it's off and rate is more than 0, turn it on
;; otherwise:
;; (knowing what minute I'm at)
;; start traversing the tunnels
;;  each move decrements the time by 1
;;  once arriving at a position, score it as follows:
;;      (30 - cur_time) * flow_rate
;;  backing up increments the time
;;
;;we're looking for the next place to move, which might be more than one away
;;  so, don't count time of opening valves, assuming we roll right through
;;  we search depth-first
;; AA 30
;;  move to BB 29
;;      open BB 28 -> score (28*13) - 364
;;          (skip AA, it's already been visited)
;;  move to DD (1 min)
;;      open DD (2 min) -> score (28*20) - 560
;;  move to II (1 min)
;;      open II (2 min) -> score 0
;;
;;  next round:
;;  from BB, move to CC (2 min)
;;      open CC (3 min) -> score (27*2) - 54
;;  from BB, skip AA, it's already visited in this tree
;;  from DD, move to CC (2 min)
;;      open CC (3 min) -> score (27*2) - 54
;;  from DD, skip AA
;;  from DD, move to EE (2 min)
;;      open EE (3 min) -> score (27*3) - 81
;;  from II, move to JJ (2 min)
;;      open JJ (3 min) -> score (26*) - 567
;;
;;
;; DD
;;
;; arguments: starting_point, current time, what's turned on
;; give me the sum of the points for turning this one on
;;      and the max of all the branches
;;

(defun add-value (key value alist)
  (acons key value alist))

(defun get-value (key alist)
  (cdr (assoc key alist :test #'equal)))

(defun update-value (key value alist)
  (setf (cdr (assoc key alist :test #'equal)) value))


(defun parse-input (file-list)
  (let ((valves)
        (turnable))
    (dolist (next file-list)
      (let ((result (cl-ppcre:split ";*,* " next)))
        (setf valves (acons (second result)
                            (list (parse-integer (second (cl-ppcre:split "=" (fifth result))))
                                  (subseq result 9))
                            valves))
        (when (< 0 (parse-integer (second (cl-ppcre:split "=" (fifth result)))))
          ;(format t "maybe turnable? ~A~%" (second result))
          (setf turnable (cons (second result) turnable)))
        ))
    (list valves turnable)))

;(second (parse-input *file*))

(defvar *valves* )
(setf *valves* (first (parse-input *file*)))
(defvar *turnable* )
(setf *turnable* (second (parse-input *file*)))


(defun get-max (values)
  (let ((max 0))
    (dolist (val values)
      (setf max (max val max)))
    max))

(defun get-min (values)
  (let ((min 1000000))
    (dolist (val values)
      (setf min (min val min)))
    min))

(defun calc-dist (start end visited)
  (let ((valve (get-value start *valves*))
        (sums)
        (score 0))
    (setf visited (cons start visited))
    (when (equal start end)
      (return-from calc-dist 0))
    (dolist (next-valve (second valve))
      (unless (member next-valve visited :test #'equal)
        (setf sums (cons (calc-dist next-valve end visited) sums))))
    (+ 1 (get-min sums))))

(defvar *distances*)

(defun calc-distances ()
  (let (distances)
  (format t "Mapping the Tunnels~%")
  (dolist (next *turnable*)
    (let (local-dists)
      (dolist (target (remove next *turnable* :test #'equal))
        (setf local-dists (acons target (calc-dist next target (list next)) local-dists)))
      (setf distances (acons next local-dists distances))))
    (let (local-dists)
      (dolist (target (remove "AA" *turnable* :test #'equal))
        (setf local-dists (acons target (calc-dist "AA" target (list "AA")) local-dists)))
      (setf distances (acons "AA" local-dists distances)))
  distances)
  )

(setf *distances* (calc-distances))

(defun get-dist (start end)
  (get-value end (get-value start *distances*)))

(get-value "AA" *distances*)
(assoc "AA" *distances* :test #'equal)

(get-dist "AA" "NT")

(defun sum-valves (start time turned-on &optional from)
  (let ((valve (get-value start *valves*))
        (sums)
        (score 0))
    ;(format t "Before the first when: ~A~%" start)
    (when (and  (not (member start turned-on :test #'equal))
                (member start *turnable* :test #'equal))
      ;(format t "In the first when~%")
      (setf score (* time (first valve)))
      (setf turned-on (cons start turned-on))
      (setf time (1- time)))
    ;(format t "After the first when~%")
    (cond
      ((equal (length turned-on) (length *turnable*))
       ;(format t "Everything turned on~%")
       (return-from sum-valves score))
      ((>= 0 time)
       ;(format t "Time has run out~%")
       (return-from sum-valves score)))
    ;(format t "Doing the list~%")
    (dolist (next-valve (second valve))
      (unless (equal next-valve from)
        (setf sums (cons (sum-valves next-valve (1- time) turned-on) sums)))
      )
    (+ score (get-max sums))
    ))

;(sum-valves "AA" 30 '())


;; that didn't work
;; what if I took the list of all valves that need to be turned on and set out to turn them all on
;; start where I am, go to the closest, turn it on, go to the next closest, repeat

(defun get-score (pos time)
  (* time (first (get-value pos *valves*))))

(defun compare-targets (left right)
  (cond
    ((< (first left) (first right))
     't)
    ((> (first left) (first right))
     nil)
    ((>= (second left) (second right))
     't)
    ((< (second left) (second right))
     nil)))

(get-dist "AA" "HH")

(defun get-answer (start time turned-on)
  (let ((valve (get-value start *valves*))
        (closest)
        (next-valve)
        (sums))
    (when (equal (length turned-on) (length *turnable*))
      (return-from get-answer 0))
    (dolist (next *turnable*)
      (unless (member next turned-on :test #'equal)
        ;(format t "Start: ~A next: ~A~%" start next)
        ;(format t "dist ~A~%" (get-dist start next))
        (let ((dist (get-dist start next)))
          (setf closest (acons next (list dist
                                        (get-score next (- time (1+ dist))))
                                        closest)))
        ))
    (setf closest (sort closest #'compare-targets :key #'cdr))
    ;(format t "~A~%" closest)
    (when (equal 1 (length closest))
      (return-from get-answer (third (car closest))))
    ;(format t "Looping on ~A: ~A~%" start closest)
    (dotimes (ix (min 3 (1- (length closest))))
      ;(format t "In ~A Loop: ~A ~A~%: " start ix (nth ix closest))
      (let* ((next-valve (nth ix closest))
             (next-time (- time (+ 1 (second next-valve))))
             (next-turned-on (cons (first next-valve) turned-on))
             )
        (setf sums (cons (+ (third next-valve)
                            (get-answer (first next-valve) next-time next-turned-on))
                    sums))))
    ;; (setf next-valve (first closest))
    ;; (format t "next-valve: ~A~%" next-valve)
    ;; (setf time (- time (+ 1 (second next-valve))))
    ;; (setf turned-on (cons (first next-valve) turned-on))
    ;; (+ (third next-valve) (get-answer (first next-valve) time turned-on))
    ;(get-answer (car ))
    (get-max sums)
    ))

(format t "Answer: ~A~%" (get-answer "AA" 30 '()))

;(get-dist "AA" "NT")
