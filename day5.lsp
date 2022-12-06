;;;; Day 5
(ql:quickload :cl-ppcre )

(defun load-file (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil)
          while line
          collect line)))

(defvar *file* nil) ;;procedure

(setf *file* (load-file "day5_input.txt")) ;; procedure
;(setf *file* (load-file "./test_input_day5.txt")) ;; procedure
;;
;;
;;columns:
;; 1 , 5 , 9
;; every 4
;; x - 1 / 4

(defun find-total-length (length-line)
     (parse-integer (car (last (cl-ppcre:split :whitespace-char-class length-line)))))

;(parse-integer (find-total-length (nth 3 *file*)))

(defun parse-container-line (container-line prev-pos)
  (let ((containers '())
        bracket-pos)
    (when (position #\[ container-line)
        (let ((start (position #\[ container-line))
              (end (position #\] container-line)))
          (setf containers (cons (list
                                   (subseq container-line (incf start) end)
                                   (+ start prev-pos))
                                 containers))
          (setf bracket-pos end)))
    (when (position #\[ (subseq container-line bracket-pos))
      (setf containers (append containers (parse-container-line
                                           (subseq container-line (incf bracket-pos))
                                           (+ bracket-pos prev-pos)))))
    containers))

;(parse-container-line (caddr *file*) 0)

(defun stack-containers (c-list x)
  (let (c-stack)
    (dolist (next-row c-list)
      (dolist (next-container next-row)
        (when (equal (cadr next-container)
                    (+ 1 (* 4 x)))
        (setf c-stack (cons (car next-container) c-stack)))))
    c-stack))

(defun parse-move (move-line)
  (let (out-line)
    (setf out-line (cl-ppcre:split :whitespace-char-class move-line :omit-unmatched-p nil))
    (mapcar 'parse-integer (list (nth 1 out-line) (nth 3 out-line) (nth 5 out-line)))))

(defun move-container (move-cmd container-stacks)
  ;(format t "Processing move cmd: ~a~%" move-cmd)
  (setf move-cmd (cons (decf (car move-cmd)) (cdr move-cmd)))
  (let ((from-column (nth (- (cadr move-cmd) 1) container-stacks))
        (to-column (nth (- (caddr move-cmd) 1) container-stacks))
        lower-column
        upper-column
        lower-bound
        upper-bound
        new-stacks '())
    ;(format t "from-column: ~a~%" from-column)
    ;(format t "to-column: ~a~%" to-column)
    (setf to-column (cons (car from-column) to-column))
    (setf from-column (cdr from-column))
    (if (< (cadr move-cmd) (caddr move-cmd))
        (progn (setf lower-bound (- (cadr move-cmd) 1))
               (setf upper-bound (- (caddr move-cmd) 1))
               (setf lower-column from-column)
               (setf upper-column to-column))
        (progn (setf lower-bound (- (caddr move-cmd) 1))
               (setf upper-bound (- (cadr move-cmd) 1))
               (setf lower-column to-column)
               (setf upper-column from-column)))
        ;(format t "uppper: ~a~%" upper-bound)
        ;(format t "uppper: ~a~%" upper-column)
        ;(format t "lower: ~a~%" lower-bound)
        ;(format t "lower: ~a~%" lower-column)
    ;; (print (subseq container-stacks 0 lower-bound))
    ;;                          (print (list lower-column))
    ;;                          (print (subseq container-stacks (+ 1 lower-bound) upper-bound))
    ;;                          (print (list upper-column))
    ;; (setf new-stacks (append (subseq container-stacks 0 lower-bound)
    ;;                          (list lower-column)
    ;;                          (subseq container-stacks (+ 1 lower-bound) upper-bound)
    ;;                          (list upper-column)))
    (dotimes (x (length container-stacks))
      ;(format t "looping at x: ~d~%" x)
      (if (or (equal x lower-bound) (equal x upper-bound))
          (if (equal x lower-bound)
              (setf new-stacks (append new-stacks (list lower-column)))
              (setf new-stacks (append new-stacks (list upper-column))))
          (setf new-stacks (append new-stacks (list (nth x container-stacks))))))
      ;(format t "new-stack: ~a~%" new-stacks)
    (if (>= 0 (car move-cmd))
      (return-from move-container new-stacks)
      (move-container move-cmd new-stacks))))

(defun init-containers (file-data move-fn)
  (let (container-list
        move-list
        (there-yet nil)
        total-length
        container-stacks)
  ;find full width
    (dolist (next-line file-data)
      (if (not there-yet)
          (if (position #\[ next-line)
              (setf container-list (cons
                                    (parse-container-line next-line 0)
                                    container-list)) ; get the containers
              (progn
                     (setf there-yet 't)
                     (setf total-length (find-total-length next-line))))
          (when (< 0 (length next-line)) (setf move-list (cons (parse-move next-line) move-list)))
      )
    )
    ; build the containers, using container-list and total-length
    (dotimes (x total-length)
      (setf container-stacks (cons (stack-containers container-list x) container-stacks)))
      (setf container-stacks (reverse container-stacks))
    ;(format t "move-list: ~a~%" (reverse move-list))
    (dolist (next-move (reverse move-list))
      (setf container-stacks (funcall move-fn next-move container-stacks)))
      ;(format t "container-stack: ~a~%" container-stacks)
    ;(move-container (car move-list) container-stacks)
    container-stacks
  ))

(defun count-output (final-stacks)
  (let ((final-string ""))
    (dolist (top final-stacks)
      (setf final-string (concatenate 'string final-string (car top))))
    final-string))


(defun move-container-9001 (move-cmd container-stacks)
  (format t "Processing move cmd: ~a~%" move-cmd)
  ;(setf move-cmd (cons (decf (car move-cmd)) (cdr move-cmd)))
  (let ((from-column (nth (- (cadr move-cmd) 1) container-stacks))
        (to-column (nth (- (caddr move-cmd) 1) container-stacks))
        lower-column
        upper-column
        lower-bound
        upper-bound
        new-stacks '())
    (setf to-column (append (subseq from-column 0 (car move-cmd)) to-column))
    ;(setf to-column (cons (car from-column) to-column))
    (setf from-column (subseq from-column (car move-cmd)))
    ;(setf from-column (cdr from-column))
    (format t "from-column: ~a~%" from-column)
    (format t "to-column: ~a~%" to-column)
    (if (< (cadr move-cmd) (caddr move-cmd))
        (progn (setf lower-bound (- (cadr move-cmd) 1))
               (setf upper-bound (- (caddr move-cmd) 1))
               (setf lower-column from-column)
               (setf upper-column to-column))
        (progn (setf lower-bound (- (caddr move-cmd) 1))
               (setf upper-bound (- (cadr move-cmd) 1))
               (setf lower-column to-column)
               (setf upper-column from-column)))
        (format t "uppper: ~a~%" upper-bound)
        (format t "uppper: ~a~%" upper-column)
        (format t "lower: ~a~%" lower-bound)
        (format t "lower: ~a~%" lower-column)
    ;; (print (subseq container-stacks 0 lower-bound))
    ;;                          (print (list lower-column))
    ;;                          (print (subseq container-stacks (+ 1 lower-bound) upper-bound))
    ;;                          (print (list upper-column))
    ;; (setf new-stacks (append (subseq container-stacks 0 lower-bound)
    ;;                          (list lower-column)
    ;;                          (subseq container-stacks (+ 1 lower-bound) upper-bound)
    ;;                          (list upper-column)))
    (dotimes (x (length container-stacks))
      (format t "looping at x: ~d~%" x)
      (if (or (equal x lower-bound) (equal x upper-bound))
          (if (equal x lower-bound)
              (setf new-stacks (append new-stacks (list lower-column)))
              (setf new-stacks (append new-stacks (list upper-column))))
          (setf new-stacks (append new-stacks (list (nth x container-stacks)))))
      (format t "new-stack: ~a~%" new-stacks))
    ;(if (>= 0 (car move-cmd))
      new-stacks))
      ;(move-container-9001 move-cmd new-stacks))))

(format t "output: ~a~%" (count-output  (init-containers *file* 'move-container-9001)))

; move 3 from 1 to 3
; decrement move
;   move the from to (0 to 2)
;
; call move 2 from 1 t- 3
; decrement move
;   move the from to
; call move 1 from 1 to 3
; decrement move
;   move the from to
;   if move is 0, return the two affected columns
;     rebuild the column list?
