;; Day12
;;
;; second array for tracking
;;  it will start out at nil
;;  if it's been visited, it will be 0
;;  if it's on the path, it will be 1
;;
;; array to hold list of visited nodes in reverse order
;;  this allows easy cons'ing and popping
;;
;; calculate the cost->distance
;;  (abs )
;;
;; on a given spot:
;;  check the neighbors:
;;      is it a valid move? what's the cost?
;;          is it valid: not an edge
;;                       distance up is not greater than one
;;                       has not already been visited
;;          what's the cost: distance from destination
;;                       subtract one from cost if it goes up?
;;                       add one to cost if it goes down?
;;                          not allowed to go down into an a
;;      choose to move to the lowest-cost, valid move
;;      mark all valid moves as checked in the array
;;      mark choice in the array
;;      append choice coords to the visited list
;;      set choice as the current spot
;;      finish
;;
;;      if no valid moves: backtrack
;;          mark current spot as visited
;;          set previous spot as current spot
;;          finish
;;
;; loop forever, until current spot is E
;;
;;


(ql:quickload :cl-ppcre )

(defun load-file (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil)
          while line
          collect line)))

(defvar *file* nil) ;;procedure

;(setf *file* (load-file "day12_input.txt")) ;; procedure
(setf *file* (load-file "./test_input_day12.txt")) ;; procedure

(defconstant big-number 1000000)

(defun parse-input (file-list)
  (let ((start-pos nil)
        (end-pos nil)
        (map (make-array (list
                          (length *file*)
                          (length (car *file*)))))
        (next)
        (ix 0))
    (dolist (next file-list)
      (let ((chr-ix 0))
      (loop for chr across (string-to-octets next) do
        (setf (aref map ix chr-ix) chr)
        (incf chr-ix)))
      (when (position #\S next)
        (setf start-pos (list ix (position #\S next)))
        (setf (aref map (first start-pos) (second start-pos)) 97))
      (when (position #\E next)
        (setf end-pos (list ix (position #\E next))) ; positions are going to be y,x
        (setf (aref map (first end-pos) (second end-pos)) 123)) ; set it one higher than z
      (incf ix))
      (list map start-pos end-pos)
    ))


(defun get-distance (here there)
  (+ (abs (- (second here) (second there)))
     (abs (- (first here) (first there)))))

(defun find-path (file-list test-limit)
  (let ((start-pos nil)
        (end-pos nil)
        (map)
        (visited)
        (path-list)
        (parsed)
        (cur)
        (test-ix 0))
    (setf parsed (parse-input file-list))
    (setf map (first parsed))
    (setf start-pos (second parsed))
    (setf end-pos (third parsed))
    (setf visited (make-array (array-dimensions map) :initial-element nil))
    (setf path-list (cons (list start-pos) path-list))
    (setf cur start-pos)
    (format t "cur: ~A~%" cur)
    (loop
        (when (< test-limit test-ix)
          (return))
      (let* ((up (mapcar #'+ '(-1 0) cur))
             (up-scr (get-score up cur end-pos map visited))
            (down (mapcar #'+ '(1 0) cur))
             (down-scr (get-score down cur end-pos map visited))
            (left (mapcar #'+ '(0 -1) cur))
             (left-scr (get-score left cur end-pos map visited))
            (right (mapcar #'+ '(0 1) cur))
             (right-scr (get-score right cur end-pos map visited))
             (best-score (apply 'min (list up-scr down-scr left-scr right-scr)))
             (choice))
        ;; (format t "up: ~A~%up-scr: ~A~%" up up-scr)
        ;; (format t "down: ~A~%down-scr: ~A~%" down down-scr)
        ;; (format t "left: ~A~%left-scr: ~A~%" left left-scr)
        ;; (format t "right: ~A~%right-scr: ~A~%" right right-scr)
        (cond
          ((equal big-number best-score)
           (backtrack )) ; TODO implement backtrack
          ((<= down-scr best-score)
           (setf choice down))
          ((<= right-scr best-score)
           (setf choice right))
          ((<= up-scr best-score)
           (setf choice up))
          ((<= left-scr best-score)
           (setf choice left))
          )
         (setf path-list (cons (list choice) path-list))
        (setf (aref visited (first cur) (second cur)) 't)
        (setf cur choice)
        (when (equal choice end-pos)
          (format t "path-list~A~%" path-list)
          (format t "visited: ~A~%" visited)
          (return-from find-path (1- (length path-list))))
        ;; (format t "chosen: ~A~%" choice)
        ;; (format t "path-list~A~%" path-list)
        (incf test-ix)
        ))
    (format t "path-list~A~%" path-list)
    (format t "visited: ~A~%" visited)
    ))

(find-path *file* 40)

(defun not-too-high (here there)
  (if (< 1 (- there here))
      nil
      't))

(defun get-score (neighbor here end-pos map visited)
  (let ((neighbor-val (ignore-errors (aref map (first neighbor) (second neighbor))))
        (is-visited (ignore-errors (aref visited (first neighbor) (second neighbor))))
        (here-val (aref map (first here) (second here))))
    ;; (format t "neighbor-val: ~A~%" neighbor-val)
    ;; (format t "is-visited: ~A~%" is-visited)
    ;; (format t "here-val: ~A~%" here-val)
  (if (and neighbor-val (and (not is-visited) (not-too-high here-val neighbor-val)))
      (let ((dist (get-distance neighbor end-pos)))
        (if (< here-val neighbor-val)
            (1- dist)
            dist))
      big-number)
  ))

;; (defvar test-results (parse-input *file*))
;; (first test-results)
;; (1+ (parse-integer #\z ))
;; (string-to-octets "z")
;; (string-to-octets "{")
