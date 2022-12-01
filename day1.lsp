; Day 1, AoC 2022

(defvar *file* nil)

(defun load-file (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil)
          while line
          collect line)))

(setf *file* (load-file "./test_input_day1.txt"))

(setq elves nil)
(setq foods nil)

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
        (setq foods (append foods (list (parse-integer food)))))))

(defun count-calories (elf-calories)
    (dolist (elf elf-calories)
      (format t "~d~%" (reduce #'+ elf))))
