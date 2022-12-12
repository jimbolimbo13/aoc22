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

(defvar old) ; needs to be defvar'd for proper use of (eval (read-from-string ( "old" )))


(defun add-value (key value alist)
  (acons key value alist))

(defun get-value (key alist)
  (cdr (assoc key alist :test #'equal)))

(defun update-value (key value alist)
  (setf (cdr (assoc key alist :test #'equal)) value))

(defun parse-monkeys (file-list)
  (let ((monkeys)
        (monkey-count 0)
        (next-monkey)
        (ix 0))
    (dolist (next-line file-list)
      (incf ix)
      ; handle lines with bad ":" locations
      (when (or (equal 1 (mod ix 7))
                (equal 0 (mod ix 7)))
        (setf next-line ": "))
      (let ((next (cl-ppcre:split
                  ",* "
                   (subseq next-line (+ 2 (position #\: next-line))))))
        (cond
          ((equal 1 (mod ix 7))
           (setf next-monkey '())
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
           (incf monkey-count)))))
    monkeys))

(defun get-monkey-items (val monkeys)
  (get-value :items  ;; thrown monkey's items
    (get-value val monkeys))) ;; monkey I'm throwing to

(defun monkey-business (monkeys relief rounds)
  (let ((monkey-inspections (make-array (length monkeys) :initial-element 0))
        (relief-fn #'(lambda (x y) (truncate (/ x y)))))
    (when (equal 1 relief)
      (setf relief-fn #'(lambda (x y) (mod x y)))
      (dotimes (ix (length monkeys))
        (setf relief (* relief
                        (get-value :divisor (get-value ix monkeys))))))
    (do ((round 0 (1+ round)))
        ((<= rounds round))
      (dotimes (mky-ix  (length monkeys))
        (let ((monkey (get-value mky-ix monkeys)))
        ;; each monkey inspects their list
        (dolist (item (get-value :items monkey))
          (let ((worry)
                (divisor (get-value :divisor monkey))
                (throw-to))
            (setf old item)
            (setf worry (funcall (read-from-string (second (get-value :operation monkey)))
                                 (eval (read-from-string (first (get-value :operation monkey))))
                                 (eval (read-from-string (third (get-value :operation monkey))))))
            (setf worry (funcall relief-fn worry relief))
            (if (equal 0 (mod worry divisor))
                (setf throw-to (get-value :throw-true monkey))
                (setf throw-to (get-value :throw-false monkey)))
            (let* ((thrown-monkey (get-value throw-to monkeys))
                   (thrown-monkey-items (cons worry (get-monkey-items throw-to monkeys))))
                  (update-value :items thrown-monkey-items thrown-monkey)
                  (update-value throw-to thrown-monkey monkeys))))
        ; update monkey inpsections count with lenght of :items
        (setf (aref monkey-inspections (get-value :monkey-num monkey))
              (+ (length (get-value :items monkey))
                 (aref monkey-inspections (get-value :monkey-num monkey))))
        (update-value :items '() monkey)
        (update-value (get-value :monkey-num monkey) monkey monkeys))))
    (let ((max1 0)
          (max2 0))
      (dotimes (ix (length monkeys))
        (let ((ins-ct (aref monkey-inspections ix)))
        (if (< max1 ins-ct)
            (progn
              (setf max2 max1)
              (setf max1 ins-ct))
            (when (< max2 ins-ct)
              (setf max2 ins-ct)))))
      (* max1 max2))))

(format t "Day11, part1: ~A~%" (monkey-business (parse-monkeys *file*) 3 20))
(format t "Day11, part2: ~A~%" (monkey-business (parse-monkeys *file*) 1 10000))
