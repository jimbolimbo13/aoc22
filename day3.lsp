;; Day 3
;; read items
;; split in half
;; split string into array possibly
;; sort alphabetically
;;  (sort '("B" "A") #'string-lessp)
;;instead of sorting alphabetically, 
;;        make them both sets
;; e.g. (adjoin 1 *myset*)
;;        then do an intersection to find the common 
;; intersection list1 list2 &key :test :test-not :key
;; (intersection '(a b c) '(c d e))
;;
;;
;; to score, get value of char and subtract the baseline
;;  (aref (string-to-octets "Z") 0)

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
