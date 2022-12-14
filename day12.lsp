;; Day12
;;


(ql:quickload :cl-ppcre )

(defun load-file (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil)
          while line
          collect line)))

(defvar *file* nil) ;;procedure

(setf *file* (load-file "day12_input.txt")) ;; procedure
;(setf *file* (load-file "./test_input_day12.txt")) ;; procedure

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
           ;(format t "backtracking from: ~A~%" cur)
           (setf choice (caar path-list))
           (setf path-list (cdr path-list)))
          ((<= down-scr best-score)
           (setf choice down))
          ((<= right-scr best-score)
           (setf choice right))
          ((<= up-scr best-score)
           (setf choice up))
          ((<= left-scr best-score)
           (setf choice left)))
        (unless (equal big-number best-score)
          (setf path-list (cons (list choice) path-list)))
        (setf (aref visited (first cur) (second cur)) 't)
        (setf cur choice)
        (sleep 0.1)
        (format t "~%~%~%~%~%~%")
          (print-output path-list file-list)
        (when (equal choice end-pos)
          (format t "path-list~A~%" path-list)
          ;(format t "visited: ~A~%" visited)
          (print-output path-list file-list)
          (return-from find-path (1- (length path-list))))
        ;; (format t "chosen: ~A~%" choice)
        ;; (format t "path-list~A~%" path-list)
        (incf test-ix)
        ))
    (format t "path-list~A~%" path-list)
    (print-output path-list file-list)
    ;(format t "visited: ~A~%" visited)
    ))


(defun print-output (path-list file-list)
  (let ((ix 0))
    (dolist (next file-list)
     (let ((sub-ix 0))
       (dolist (sub-next (coerce next 'list))
         (if (member (list (list ix sub-ix)) path-list :test #'equal)
             (format t "X")
             (format t "~A" sub-next))
         (incf sub-ix)))
      (format t "~%")
      (incf ix))))

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
  ;; (when (and (equal 99 here-val) (equal 97 neighbor-val)) ;don't go down from c to a. It's a trap
  ;;   (return-from get-score big-number))
  (if (and neighbor-val (and (not is-visited) (not-too-high here-val neighbor-val)))
      (let ((dist (get-distance neighbor end-pos)))
        (cond
         ((< here-val neighbor-val)
            (1- dist))
         ((> here-val neighbor-val)
          (1+ dist))
         ('t dist)))
      big-number)
  ))

;(format t "Day12 part1: ~A~%" (find-path *file* 40))

(format t "Day12 part1: ~A~%" (find-path *file* 500))
