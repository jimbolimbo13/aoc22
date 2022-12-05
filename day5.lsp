;;;; Day 5
(ql:quickload :cl-ppcre )

(defun load-file (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil)
          while line
          collect line)))

(defvar *file* nil) ;;procedure

;(setf *file* (load-file "day5_input.txt")) ;; procedure
(setf *file* (load-file "./test_input_day5.txt")) ;; procedure
;;
;;
;;columns:
;; 1 , 5 , 9
;; every 4
;; x - 1 / 4

(defun find-total-length (length-line)
     (parse-integer (car (last (cl-ppcre:split :whitespace-char-class length-line)))))

(parse-integer (find-total-length (nth 3 *file*)))

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

(parse-container-line (caddr *file*) 0)

(defun init-containers (file-data)
  (let (container-list
        move-list
        (there-yet nil)
        total-length)
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
          (format t "count the lines")
      )
    )
    ; build the containers, using container-list and total-length
    container-list
  ))
