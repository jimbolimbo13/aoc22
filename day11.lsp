;; Day11
;;
;;


(ql:quickload :cl-ppcre )

(defun load-file (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil)
          while line
          collect line)))

(defvar *file* nil) ;;procedure

(setf *file* (load-file "day11_input.txt")) ;; procedure
;(setf *file* (load-file "./test_input_day11.txt")) ;; procedure

(defun add-value (key value alist)
  (acons key value alist))

(defun get-value (key alist)
  (cdr (assoc key alist :test #'equal)))

(defun update-value (key value alist)
  (setf (cdr (assoc key alist :test #'equal)) value))

(defun worry-calc (operation)
  (funcall (read-from-string (second operation))
           (eval (read-from-string (first operation)))
           (eval (read-from-string (third operation)))))

(defun parse-monkeys (file-list)
  (let ((monkeys)
        (monkey-count 0)
        (next-monkey)
        (ix 0))
    (dolist (next-line file-list)
      (incf ix)
      ;(format t "index: ~A~%" ix)
      ; handle lines with bad ":" locations
      (when (or (equal 1 (mod ix 7))
                (equal 0 (mod ix 7)))
        (setf next-line ": "))
      (let ((next (cl-ppcre:split
                  ",* "
                   (subseq next-line (+ 2 (position #\: next-line))))))
        ;(format t "next-line: ~A~%" next-line)
        (cond
          ((equal 1 (mod ix 7))
           (setf next-monkey '())
           ;(setf next-monkey (acons :monkey-num monkey-count alist))
           (setf next-monkey (add-value :monkey-num monkey-count next-monkey))
           )
          ((equal 2 (mod ix 7))
           (setf next-monkey (add-value :items (mapcar #'parse-integer next) next-monkey)))
          ((equal 3 (mod ix 7))
           (setf next-monkey (add-value :operation (subseq next 2) next-monkey)))
          ((equal 4 (mod ix 7))
           (setf next-monkey (add-value :divisor (parse-integer (third next)) next-monkey)))
          ((equal 5 (mod ix 7))
           (setf next-monkey (add-value :throw-true (parse-integer (fourth next)) next-monkey)))
          ((equal 6 (mod ix 7))
           (setf next-monkey (add-value :throw-false (parse-integer (fourth next)) next-monkey))
           (setf monkeys (add-value monkey-count next-monkey monkeys))
           (incf monkey-count))
          )))
    ;(format t "parsed monkeys:~%~A~%" monkeys)
    monkeys
    ))

(parse-monkeys *file*)

(defun get-monkey-items (val monkeys)
  (get-value :items  ;; thrown monkey's items
    (get-value val monkeys) ;; monkey I'm throwing to
                        ))
(defvar old) ; needs to be defvar'd for proper use of (eval (read-from-string ( "old" )))

(defun monkey-business (monkeys)
  (let ((monkey-inspections (make-array (length monkeys) :initial-element 0)
        ))
    (format t "")
    (do ((round 0 (1+ round)))
        ((<= 20 round))
        ;((<= 1 round))
      (dotimes (mky-ix  (length monkeys))
        (let ((monkey (get-value mky-ix monkeys)))
        ;(format t "monkey: ~A~%" monkey)
        ;; each monkey inspects their list
        (dolist (item (get-value :items monkey))
          (let ((worry)
                (divisor (get-value :divisor monkey))
                (throw-to))
            (setf old item)
            ;; (format t "old: ~A~%" old)
            ;; (format t "~A~%" (eval (read-from-string (first (get-value :operation monkey)))))
            ;; (format t "~A~%" (read-from-string (third (get-value :operation monkey))))
            (setf worry (funcall (read-from-string (second (get-value :operation monkey)))
                                 (eval (read-from-string (first (get-value :operation monkey))))
                                 (eval (read-from-string (third (get-value :operation monkey))))))
            (setf worry (truncate (/ worry 3)))
            (if (equal 0 (mod worry divisor))
                (setf throw-to (get-value :throw-true monkey))
                (setf throw-to (get-value :throw-false monkey)))
            (let* ((thrown-monkey (get-value throw-to monkeys))
                   (thrown-monkey-items (cons worry (get-monkey-items throw-to monkeys))))
              ;; (format t "throwing item: ~A to monkey ~A~%" item throw-to)
              ;; (format t "~A~%" thrown-monkey)
                  (update-value :items thrown-monkey-items thrown-monkey)
                  (update-value throw-to thrown-monkey monkeys))
            ))
        ; update monkey inpsections count with lenght of :items
        (setf (aref monkey-inspections (get-value :monkey-num monkey))
              (+ (length (get-value :items monkey))
                 (aref monkey-inspections (get-value :monkey-num monkey))))
        (update-value :items '() monkey)
        (update-value (get-value :monkey-num monkey) monkey monkeys))
          ;(setf (aref tracker from-top-y outer-loop) 1)
        ))
    (let ((max1 0)
          (max2 0))
      (dotimes (ix (length monkeys))
        (let ((ins-ct (aref monkey-inspections ix)))
        (if (< max1 ins-ct)
            (progn
              (setf max2 max1)
              (setf max1 ins-ct))
            (when (< max2 ins-ct)
              (setf max2 ins-ct)))
      ))
      (* max1 max2))
    )
  )

(format t "Day11, part1: ~A~%" (monkey-business (parse-monkeys *file*)))
;; (mapcar #'parse-integer (cl-ppcre:split ",* "
;;  (subseq (second *file*) (+  2 (position #\: (second *file*))))))

;; (defvar left)
;; (defvar right)

;; (setf left 2)
;; (setf right 5)

;; (setf in-left (read-from-string "left"))

;; (funcall #'*
;;          (read-from-string "left")
;;          (read-from-string "right"))

;; (numberp (read-from-string "19"))

;; (funcall (read-from-string "*")
;;          (eval (read-from-string "left"))
;;          (eval (read-from-string "right")))

(fourth (cl-ppcre:split
        ",* "
        (subseq (sixth *file*) (+ 2 (position #\: (sixth *file*)))))
      )
