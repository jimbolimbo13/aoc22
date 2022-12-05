;; Day2

(defun load-file (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil)
          while line
          collect line)))

(defun split-line (full-line)
  (list
   (subseq full-line 0 1)
   (subseq full-line 2 3)))

;; A - Rock, B- Paper, C-Scissors
;; X-Rock (1), Y - Paper (2), Z - Scissors (3)
(defun score-round-part1 (round)
  (let (;(score 0)
        (loss-pt 0)
        (draw-pt 3)
        (win-pt 6)
        (rock-pt 1)
        (paper-pt 2)
        (scissors-pt 3))
    (cond
      ((equal "A" (car round))
        (cond
              ;("X" (setq score (+ rock-pt draw-pt))) ;draw
              ((equal "X" (cadr round)) (+ rock-pt draw-pt)) ;draw
              ((equal "Y" (cadr round)) (+ paper-pt win-pt)) ; win
              ((equal "Z" (cadr round)) (+ scissors-pt loss-pt)))) ; loss
      ((equal"B" (car round))
       (cond
              ((equal "X" (cadr round)) (+ rock-pt loss-pt))
              ((equal "Y" (cadr round)) (+ paper-pt draw-pt))
              ((equal "Z" (cadr round)) (+ scissors-pt win-pt))))
      ((equal "C" (car round))
       (cond
              ((equal "X" (cadr round)) (+ rock-pt win-pt))
              ((equal "Y" (cadr round)) (+ paper-pt loss-pt))
              ((equal "Z" (cadr round)) (+ scissors-pt draw-pt)))))))

(defun file-to-moves (file-data)
  (let ((moves-list '()))
    (dolist (line file-data)
      (setq moves-list (append moves-list (list (split-line line)))))
    moves-list))

(defun total-of-rounds (round-list score-fn)
  (let ((grand-total 0))
    (dolist (round round-list)
      (setq grand-total (+ grand-total (funcall score-fn round))))
    grand-total))

(defvar *file* nil) ;;procedure

(setf *file* (load-file "day2_input.txt")) ;; procedure
;(setf *file* (load-file "./test_input_day2.txt")) ;; procedure

(format t "Day2 Part1 answer: ~d~%" (total-of-rounds (file-to-moves *file*) 'score-round-part1)) ;;procedure


;; Part 2


;; A - Rock, B- Paper, C-Scissors
;; X-Lose , Y - draw , Z - win
(defun score-round-part2 (round)
  (let (;(score 0)
        (loss-pt 0)
        (draw-pt 3)
        (win-pt 6)
        (rock-pt 1)
        (paper-pt 2)
        (scissors-pt 3))
    (cond
      ((equal "A" (car round))
        (cond
              ((equal "X" (cadr round)) (+ scissors-pt loss-pt))
              ((equal "Y" (cadr round)) (+ rock-pt draw-pt))
              ((equal "Z" (cadr round)) (+ paper-pt win-pt))))
      ((equal"B" (car round))
       (cond
              ((equal "X" (cadr round)) (+ rock-pt loss-pt))
              ((equal "Y" (cadr round)) (+ paper-pt draw-pt))
              ((equal "Z" (cadr round)) (+ scissors-pt win-pt))))
      ((equal "C" (car round))
       (cond
              ((equal "X" (cadr round)) (+ paper-pt loss-pt))
              ((equal "Y" (cadr round)) (+ scissors-pt draw-pt))
              ((equal "Z" (cadr round)) (+ rock-pt win-pt)))))))

(format t "Day2 Part2 answer: ~d~%" (total-of-rounds (file-to-moves *file*) 'score-round-part2)) ;;procedure
