; Day 1, AoC 2022


(defun load-file (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil)
          while line
          collect line)))


(defun append-elves (foods)
  (setq elves (append elves (list foods)))
  (setf foods nil))

(defun split-elves (input-list)
  (dolist (food input-list)
    (if (equal food "")
        ;(format t "New item of ~a~%" food)
        (append-elves foods))
    (if (equal food "")
        (setq foods nil)
        (setq foods (append foods (list (parse-integer food))))))
  (append-elves foods)
  )


(defun max-calories (elf-calories)
  (let ((highest-cal 0))
    (dolist (elf elf-calories)
      (if (> (reduce #'+ elf) highest-cal)
          ;; (format t "New Max~%")
          ;; (format t "Stays the same~%"))
          (setq highest-cal (reduce #'+ elf)))
      ;;(format t "Current calories: ~d~%" (reduce #'+ elf))
      )
    ;;(format t "Highest Calories are: ~d~%" highest-cal)
    highest-cal))


(defvar *file* nil) ;;procedure

(setf *file* (load-file "day1_input.txt ")) ;; procedure

(setq elves nil) ;; procedure
(setq foods nil) ;; procedure

(split-elves *file*) ;; procedure

(format t "Part 1 answer: ~d~%"(max-calories elves)) ;;procedure
