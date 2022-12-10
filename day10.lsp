;; Day10

(ql:quickload :cl-ppcre )

(defun load-file (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil)
          while line
          collect line)))

(defvar *file* nil) ;;procedure

(setf *file* (load-file "day10_input.txt")) ;; procedure
;(setf *file* (load-file "./test_input_day10.txt")) ;; procedure

(defun clock-tick (x-reg v-reg state cycle-count crt-count next-sum next-instr)
  (let ((signal-strength nil))
    (when (equal cycle-count next-sum)
      (setf signal-strength (* cycle-count x-reg)))
    (if (<= (abs (- x-reg crt-count)) 1)
        (format t "#")
        (format t "."))
    (if (eql :executing state)
        (progn
          (setf x-reg (+ x-reg v-reg))
          (setf v-reg nil)
          (setf state :finished-execution))
        (progn
          (when (equal "addx" (first next-instr))
            (setf v-reg (parse-integer (second next-instr)))
            (setf state :executing))))
    (list x-reg v-reg signal-strength state)))

(defun run-cpu-2 (file-list first-sum sum-interval)
  (let ((cycle-count 1)
        (crt-count 0)
        (instr-ix 0)
        (next-sum first-sum)
        (signal-sum 0)
        (exec-state :finished-execution))
    (do ((x-reg 1)
         (v-reg nil)
         (signal-strength)
         (returns '()))
        ((equal :program-terminated exec-state))
        (setf returns (clock-tick
                       x-reg v-reg exec-state cycle-count crt-count next-sum
                       (cl-ppcre:split :whitespace-char-class (nth instr-ix file-list))))
      (setf x-reg (first returns))
      (setf v-reg (second returns))
      (when (third returns)
        (setf signal-sum (+ signal-sum (third returns)))
        (setf next-sum (+ next-sum sum-interval))
        )
      (when (equal :finished-execution (fourth returns))
        (incf instr-ix))
      (setf exec-state (fourth returns))
      (when (>= instr-ix (length file-list))
        (setf exec-state :program-terminated))
      (incf cycle-count)
      (if (>= crt-count 39)
          (progn
            (setf crt-count 0)
            (format t "~%"))
          (incf crt-count))
      )
    signal-sum))


(format t "Day10 Part2:~%~%")
(format t "~%~%Day10 part1: ~a~%" (run-cpu-2 *file* 20 40))
