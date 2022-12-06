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

(defun find-total-length (length-line)
     (parse-integer (car (last (cl-ppcre:split :whitespace-char-class length-line)))))

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

(defun print-stacks (container-stacks)
  ; get longest stack
  ; starting at the top, go through each stack at that row
  ; if the stack is tall enough, get the value from the stack
  ; print out [<value>]
  ; if the stack is not tall enough, print whitespace
  (let ((tallest 0)
        (out-line ""))
    (dolist (stack container-stacks)
      (when (> (length stack) tallest)
        (setf tallest (length stack))))
    ;(format t "tallest: ~a~%" tallest)
    (format t "~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%")
    (dotimes (row tallest)
      ;(format t "row: ~a~%" (- tallest row))
      (dolist (stack container-stacks)
        ;(format t "stack height ~d~%" (length stack))
        ;(format t "stack  ~a~%"  stack)
        ;(format t "next-box: ~a~%" (nth row stack))
        (if (<= (- tallest row) (length stack))
          ;(concatenate 'string out-line "[" "N" "] ")
        ;(if (<= 1 (length stack))
          (setf out-line (concatenate 'string out-line "[" (nth (- (- tallest row) 1) (reverse stack)) "] "))
          ;(concatenate 'string out-line "[" (nth row stack) "] ")
          (setf out-line (concatenate 'string out-line "    "))
          )
        ;(format t "out-line ~a~%" out-line)
        )
      (format t "~a~%" out-line)
      (setf out-line ""))
    (sleep 0.1)))
  ;(format t "~a~%" container-stacks)))

;(format t "output: ~a~%" (init-containers *file* 'move-container-9001))

(defun move-container (move-cmd container-stacks)
  (setf move-cmd (cons (decf (car move-cmd)) (cdr move-cmd)))
  (let ((from-column (nth (- (cadr move-cmd) 1) container-stacks))
        (to-column (nth (- (caddr move-cmd) 1) container-stacks))
        lower-column
        upper-column
        lower-bound
        upper-bound
        new-stacks)
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
    (dotimes (x (length container-stacks))
      (if (or (equal x lower-bound) (equal x upper-bound))
          (if (equal x lower-bound)
              (setf new-stacks (append new-stacks (list lower-column)))
              (setf new-stacks (append new-stacks (list upper-column))))
          (setf new-stacks (append new-stacks (list (nth x container-stacks))))))
    (print-stacks new-stacks)
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
    ;(print-stacks container-stacks)
    ;(return-from init-containers)
    (dolist (next-move (reverse move-list))
      (setf container-stacks (funcall move-fn next-move container-stacks)))
    container-stacks
  ))

(defun count-output (final-stacks)
  (let ((final-string ""))
    (dolist (top final-stacks)
      (setf final-string (concatenate 'string final-string (car top))))
    final-string))


(defun move-container-9001 (move-cmd container-stacks)
  (let ((from-column (nth (- (cadr move-cmd) 1) container-stacks))
        (to-column (nth (- (caddr move-cmd) 1) container-stacks))
        lower-column
        upper-column
        lower-bound
        upper-bound
        new-stacks)
    (setf to-column (append (subseq from-column 0 (car move-cmd)) to-column))
    (setf from-column (subseq from-column (car move-cmd)))
    (if (< (cadr move-cmd) (caddr move-cmd))
        (progn (setf lower-bound (- (cadr move-cmd) 1))
               (setf upper-bound (- (caddr move-cmd) 1))
               (setf lower-column from-column)
               (setf upper-column to-column))
        (progn (setf lower-bound (- (caddr move-cmd) 1))
               (setf upper-bound (- (cadr move-cmd) 1))
               (setf lower-column to-column)
               (setf upper-column from-column)))
    (dotimes (x (length container-stacks))
      (if (or (equal x lower-bound) (equal x upper-bound))
          (if (equal x lower-bound)
              (setf new-stacks (append new-stacks (list lower-column)))
              (setf new-stacks (append new-stacks (list upper-column))))
          (setf new-stacks (append new-stacks (list (nth x container-stacks))))))
    (print-stacks new-stacks)
    new-stacks))

(format t "output: ~a~%" (count-output  (init-containers *file* 'move-container)))
(format t "output: ~a~%" (count-output  (init-containers *file* 'move-container-9001)))
