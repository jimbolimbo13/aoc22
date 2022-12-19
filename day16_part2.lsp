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

(defun get-answer (start-me start-el me-ct-down el-ct-down time turned-on)
  (let (
        (valve-me (get-value start-me *valves*))
        (valve-el (get-value start-el *valves*))
        (closest-me)
        (closest-el)
        (next-valve-me)
        (next-valve-el)
        (me-moving)
        (el-moving)
        (sums))
    ;(format t "Getting answer from: ~A ~A with ~A~%" start-me start-el turned-on)
    ;(format t "me-ct-down: ~A~%" me-ct-down)
    ;(format t "el-ct-down: ~A~%" el-ct-down)
    (when (< 0 me-ct-down) (setf me-moving 't))
    (when (< 0 el-ct-down) (setf el-moving 't))
    (when (equal (length turned-on) (length *turnable*))
      ;(format t "returning: all levers pushed. ~A ~A ~A~%" start-me start-el turned-on)
      (let ((score-me (get-score start-me time))
            (score-el (get-score start-el time)))
      ;(return-from get-answer (+ score-me score-el))))
      (return-from get-answer 0)))
    ; closest for me
    (if me-moving
        (progn
          (decf me-ct-down)
          (setf closest-me (acons start-me (list me-ct-down 0) closest-me))
         )
        (progn
         (dolist (next *turnable*)
          (unless (member next turned-on :test #'equal)
            (unless (equal start-me next)
            ;(format t "Start: ~A next: ~A~%" start next)
            ;(format t "dist-me ~A: ~A ~A~%" (get-dist start-me next) start-me next)
            (let ((dist (get-dist start-me next)))
              (setf closest-me (acons next (list dist
                                                 (get-score next (- time (1+ dist))))
                                      closest-me)))
            )))
         ()))
    ; closest for elephant
    (if (< 0 el-ct-down)
        (progn
         ;(setf closest-el (list mt-ct-down ))
         (decf el-ct-down)
         (setf closest-el (acons start-el (list el-ct-down 0) closest-el)))
        (dolist (next *turnable*)
          (unless (member next turned-on :test #'equal)
            (unless (equal start-el next)
            ;(format t "Start: ~A next: ~A~%" start next)
            ;(format t "dist-el ~A~%" (get-dist start-el next))
            (let ((dist (get-dist start-el next)))
              (setf closest-el (acons next (list dist
                                                 (get-score next (- time (1+ dist))))
                                      closest-el)))
            ))))
    (setf closest-me (sort closest-me #'compare-targets :key #'cdr))
    (setf closest-el (sort closest-el #'compare-targets :key #'cdr))
    ;(format t "closest-me: ~A~%" closest-me)
    ;(format t "closest-el: ~A~%" closest-el)
    (cond
      ((and (equal 1 (length closest-me)) (not (or el-moving me-moving)))
       ;(format t "returning on short closest-me: ~A ~A ~A~%" closest-me closest-el turned-on)
       (return-from get-answer (max (third (car closest-me)) (third (car closest-el)))))
      ((and (equal 1 (length closest-el)) (not (or el-moving me-moving)))
       ;(format t "returning on short closest-el: ~A ~A ~A~%" closest-el closest-me turned-on)
       (return-from get-answer (third (car closest-el)))))
    ;; (when (equal 1 (length closest))
    ;;   (return-from get-answer (third (car closest))))
    ;(format t "Looping on ~A: ~A~%" start-me closest-me)
    ;(format t "And on ~A: ~A~%" start-el closest-el)
    ;(dotimes (ix (min 3 (1- (length closest-me))))
    (loop named outer for ix-me from 0 to (min 4 (1- (length closest-me))) do
      ;(dotimes (ix (min 3 (1- (length closest-el))))
      (loop named inner for ix-el from 0 to (min 4 (1- (length closest-me))) do
        (unless (equal (nth ix-me closest-me) (nth ix-el closest-el))
          (unless (and (nth ix-me closest-me) (nth ix-el closest-el))
            (return-from inner))
          ;(return-from inner))
        ;(format t "In ~A Loop: ~A ~A~%" start-me (nth ix-me closest-me) (nth ix-el closest-el))
      ;(format t "In ~A Loop: ~A ~A~%" start-el ix-el (nth ix-el closest-el))
        (let* ((next-valve-me (nth ix-me closest-me))
               (next-valve-el (nth ix-el closest-el))
               (ct-dwn-next-me (if (equal 0 me-ct-down)
                                   (second next-valve-me)
                                   me-ct-down))
               (ct-dwn-next-el (if (equal 0 el-ct-down)
                                   (second next-valve-el)
                                   el-ct-down))
               (next-time (1- time))
               (next-turned-on turned-on)
               (me-score (if (equal 0 me-ct-down)
                             (third next-valve-me)
                             0))
               (el-score (if (equal 0 el-ct-down)
                             (third next-valve-el)
                             0))
             )
          ;(format t "finished the lets~%")
          (unless me-moving
            (setf next-turned-on (cons (first next-valve-me) next-turned-on)))
          (unless el-moving
            (setf next-turned-on (cons (first next-valve-el) next-turned-on)))
          (when (or (and (member (first next-valve-me) turned-on :test #'equal)
                         (not me-moving))
                    (and (member (first next-valve-el) turned-on :test #'equal)
                         (not el-moving)))
            ;(format t "Skipping this loop: ~A in ~A~%" )
            (return-from inner))
          (setf sums (cons (+ me-score el-score
                            (get-answer (first next-valve-me)
                                        (first next-valve-el)
                                        ct-dwn-next-me
                                        ct-dwn-next-el
                                        next-time
                                        next-turned-on))
                    sums))))))
    ;; (setf next-valve (first closest))
    ;; (format t "next-valve: ~A~%" next-valve)
    ;; (setf time (- time (+ 1 (second next-valve))))
    ;; (setf turned-on (cons (first next-valve) turned-on))
    ;; (+ (third next-valve) (get-answer (first next-valve) time turned-on))
    (when (and (equal start-me start-el) (equal start-me "AA"))
      (format t "max sum from: ~A ~A~%" sums (get-max sums)))
    ;(get-answer (car ))
    ;(format t "max sum from: ~A ~A~%" sums (get-max sums))
    (get-max sums)
    ))

(format t "~%Answer: ~A~%" (get-answer "AA" "AA" 0 0 26 '()))

;(get-dist "AA" "NT")
