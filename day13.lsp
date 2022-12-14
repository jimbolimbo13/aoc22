;; Day13
;;
;;

(defun load-file (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil)
          while line
          collect line)))

(defvar *file* nil) ;;procedure

(setf *file* (load-file "day13_input.txt")) ;; procedure
;(setf *file* (load-file "./test_input_day13.txt")) ;; procedure

(defun parse-input-1 (file-list)
  (let ((pkt-pairs)
        (next-pair)
        (ix 1))
    (dolist (next file-list)
      (case (mod ix 3)
        (1 (setf next-pair (list (first (listify next)))))
        (2 (setf next-pair (cons (first (listify next)) next-pair)))
        (0 (setf pkt-pairs (cons (reverse next-pair) pkt-pairs))))
      (incf ix))
    (setf pkt-pairs (cons (reverse next-pair) pkt-pairs))
    (reverse pkt-pairs)))



(defun listify (sig-str &optional start-ix)
  ; will need to retun the list along with whatever index it left off at
  ; need to be aware that [] becomes just NIL. Thankfully, [[]] at least is (NIL)
  ; listp will always be true of
  (let ((chars (coerce sig-str 'list))
        (state :lst)
        (num-str "")
        (num-val nil)
        (sig-list '())
        (results))
    (unless start-ix (setf start-ix 1))
    (do ((ix start-ix (1+ ix))
         (new-char (nth start-ix chars) (nth (1+ ix) chars)))
        ((<= (length chars) ix))
      (cond
        ((equalp #\[ new-char)
         (setf num-str "")
         (when (equal :num state)
           (setf sig-list (cons (parse-integer num-str) sig-list))
           (setf num-str ""))
         (setf state :lst)
         (setf results (listify sig-str (1+ ix)))
         (setf sig-list (cons (first results) sig-list))
         (setf ix (second results)))
        ((equalp #\, new-char)
         (if (equal :num state)
             (progn
               (setf sig-list (cons (parse-integer num-str) sig-list))
               (setf num-str "")
               (setf state :comma))))
        ((equalp #\] new-char)
         (when (equal :num state)
           (setf sig-list (cons (parse-integer num-str) sig-list))
           (setf num-str "")
           (setf state :lst-end))
         (return-from listify (list (reverse sig-list) ix)))
        ((ignore-errors (parse-integer (format nil "~A" new-char)))
         (setf num-str (concatenate 'string num-str (format nil "~A" new-char)))
         (setf state :num))))))


(defun compare-halves (left right)
  (let ((ix 0))
    (loop
      (cond
        ((and (not (nth ix left)) (nth ix right)) ; left is shorter
         (return-from compare-halves 't))
        ((and (nth ix left) (not (nth ix right))) ; right is shorter
         (return-from compare-halves nil))
        ((>= ix (length left)) ; looped through everything and they're the same length
         (return-from compare-halves :keep-trying))
        ((and (integerp (nth ix left)) (integerp (nth ix right))) ; both are integers
         (when (> (nth ix left) (nth ix right))
           (return-from compare-halves nil))
         (when (< (nth ix left) (nth ix right))
           (return-from compare-halves 't)))
        ((and (listp (nth ix left)) (listp (nth ix right))) ; both are lists -> call compare on them
         (let ((result (compare-halves (nth ix left) (nth ix right))))
           (if (not (equal  :keep-trying result))
               (return-from compare-halves result))))
        ((and (listp (nth ix left)) (integerp (nth ix right))) ; left is list, right is int -> call compare
         (let ((result (compare-halves (nth ix left) (list (nth ix right)))))
           (if (not (equal  :keep-trying result))
               (return-from compare-halves result))))
        ((and (integerp (nth ix left)) (listp (nth ix right))) ; left is int, right is list -> call compare
         (let ((result (compare-halves (list (nth ix left)) (nth ix right))))
           (if (not (equal  :keep-trying result))
               (return-from compare-halves result)))))
      (incf ix)))
  (format t "made it past the loop")
  't) ;if you've made it this far, something went wrong



(defun validate (packets)
  (let ((correct '()))
    (dotimes (ix (length packets))
      (when (compare-halves (first (nth ix packets)) (second (nth ix packets)))
        (setq correct (cons (1+ ix) correct))))
    (loop for total = 0 then (funcall #'+ total next)
          for next in correct
          finally (return total))))

(format t "Day13 part1: ~A~%" (validate (parse-input-1 *file*)))


(defun parse-input-2 (file-list)
  (let ((pkt-pairs))
    (dolist (next file-list)
      (unless (equal 0 (length next))
        (setf pkt-pairs (cons (first (listify next)) pkt-pairs))))
    (setf pkt-pairs (cons (first (listify "[[2]]")) pkt-pairs))
    (setf pkt-pairs (cons (first (listify "[[6]]")) pkt-pairs))
    pkt-pairs))


(defun find-dividers (packet-list)
  (let ((sorted-pkt (sort packet-list #'compare-halves))
        (div1 '((2)))
        (div2 '((6)))
        (ix_1 nil)
        (ix_2 nil)
        (ix 0))
    (loop
      (when (equalp div1 (nth ix sorted-pkt))
        (setf ix_1 (1+ ix)))
      (when (equalp div2 (nth ix sorted-pkt))
        (setf ix_2 (1+ ix))
        (return))
      (incf ix))
    (* ix_1 ix_2)))

(format t "Day13 Part2: ~A~%" (find-dividers (parse-input-2 *file*)))
