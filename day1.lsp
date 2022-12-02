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

(setf *file* (load-file "day1_input.txt")) ;; procedure
;(setf *file* (load-file "./test_input_day1.txt")) ;; procedure

(setq elves nil) ;; procedure
(setq foods nil) ;; procedure

(split-elves *file*) ;; procedure

(format t "Part 1 answer: ~d~%"(max-calories elves)) ;;procedure

;; Part 2

(defun insert-elf (new-elf old-list)
  (let ((output-list nil))
    (dolist (old-elf old-list)
      ;(format t "Iterating list at: ~a vs ~a~%" old-elf new-elf)
      (if (> new-elf old-elf)
          (progn
            ;(format t "Inserting new elf: ~a~%" new-elf)
            (setq output-list (cons new-elf output-list))
            ;;(append output-list (list new-elf))
            (setq new-elf old-elf)
            )
          (setq output-list (cons old-elf output-list))
      ))
    (reverse output-list)))

;(print (insert-elf 3 (list 3 2 1)))

(defun max-calories-2 (elf-calories)
  (let ((highest-cals (list 0 0 0)))
    ;; if reduction is greater than the smallest member of highest-cals, we need to add it
    ;; else keep going
    (dolist (elf elf-calories)
      (if (> (reduce #'+ elf) (car (last highest-cals)))
          (setq highest-cals (insert-elf (reduce #'+ elf) highest-cals)))

    )
    highest-cals))


(format t "Part 2 answer: ~d~%"(reduce #'+ (max-calories-2 elves))) ;;procedure
