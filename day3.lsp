;; Day 3

(defun load-file (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil)
          while line
          collect line)))

(defvar *file* nil) ;;procedure

(setf *file* (load-file "day3_input.txt")) ;; procedure
;(setf *file* (load-file "./test_input_day3.txt")) ;; procedure

(defun split-rucksack (file-list)
  (let ((split-list) (split-sack ""))
    (dolist (sack file-list)
      (setq split-sack ( list
                        (string-to-octets (subseq sack 0 (/ (length sack) 2)))
                        (string-to-octets (subseq sack (/ (length sack) 2)))
                        ))
      (setq split-list (append split-list (list split-sack))))
    split-list))

(defun find-common (rucksack-conts)
  (let ((left)
        (right))
      (dotimes (item (length (car rucksack-conts)))
        (setq left (adjoin (aref (car rucksack-conts) item) left)))
      (dotimes (item (length (cadr rucksack-conts)))
        (setq right ( adjoin (aref (cadr rucksack-conts) item) right)))
    (car (intersection left right))))

(defun calculate-score (rucksack-item)
  (let ((score 0))
    (setq score
        (if (> rucksack-item 91)
            (- rucksack-item 96)
            (- rucksack-item 38)))
    score))

(defun collect-scores (rucksack-list)
  (let ((running-count 0))
    (dolist (rucksack rucksack-list)
      (setq running-count
            (+ running-count
               (calculate-score
                (find-common rucksack)))))
    running-count))

(format t "Day3 part1 Answer: ~d~%" (collect-scores (split-rucksack *file*)))

;; Part 2

;(defvar *file* nil) ;;procedure

;(setf *file* (load-file "day3_input.txt")) ;; procedure
;(setf *file* (load-file "./test_input_day3.txt")) ;; procedure

(defun collect-groups (file-list)
  (let ((group-list))
    (dotimes (x (/ (length file-list) 3))
      (setq group-list (cons ( list
                                (string-to-octets (nth (* x 3) file-list))
                                (string-to-octets (nth (+ (* x 3) 1) file-list))
                                (string-to-octets (nth (+ (* x 3) 2) file-list))
                                )
                              group-list)))
    group-list))

(defun find-common-badge (badge-groups)
  (let ((left)
        (right)
        (center))
      (dotimes (item (length (car badge-groups)))
        (setq left (adjoin (aref (car badge-groups) item) left)))
      (dotimes (item (length (cadr badge-groups)))
        (setq center ( adjoin (aref (cadr badge-groups) item) center)))
      (dotimes (item (length (caddr badge-groups)))
        (setq right ( adjoin (aref (caddr badge-groups) item) right)))
    (car (intersection (intersection left right) center))))

(defun collect-badge-scores (badge-group-list)
  (let ((running-count 0))
    (dolist (badge-group badge-group-list)
      (setq running-count
            (+ running-count
               (calculate-score
                (find-common-badge badge-group)))))
    running-count))

(format t "Day3 part2 Answer: ~d~%" (collect-badge-scores (collect-groups *file*)))
