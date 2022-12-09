;; Day7
;;

(defvar pwd nil)
(setf pwd nil)
(defvar root nil)
(setf root nil)
(defvar prev nil)
(setf prev nil)
(defvar cur nil)
(setf cur nil)
(defvar *totals* nil)
(setf *totals* nil)
(defvar part1-limit 100000)

(defun add-value (key value alist)
  (acons key value alist))

(defun get-value (key alist)
  (cdr (assoc key alist :test #'equal)))

(defun update-value (key value alist)
  (setf (cdr (assoc key alist :test #'equal)) value))

(defun new-pwd (dir-name)
  (setf *totals* (add-value dir-name 0 *totals*))
  (list (cons "dir" dir-name)))

(defun total-key (postfix)
  (concatenate 'string "total" "-" postfix))

(defun new-dir (prev cur)
  ; just concatenates two strings. Less verbose
  (concatenate 'string prev "-" cur))

(defun get-prev-from-cur (cur)
  ; get the parent alist from cur string
  (let ((prev (subseq cur 0 (position #\- cur :from-end 't))))
  (if (equal "/" prev)
      (return-from get-prev-from-cur root)
      (get-value prev (get-prev-from-cur prev))
  )))

(defun update-prev-alist (pwd cur new-total &optional final)
  ; probably recursive, doesn't actually update?
  ; returns current alist inserted into it's position in the previous alist
  ; updates totals as it goes
  (let ((parent (get-prev-from-cur cur))
        (prev (subseq cur 0 (position #\- cur :from-end 't))))
    (when (equal "/" cur) ; no parent means cur is the root
        (setf root pwd)
        (return-from update-prev-alist))
    (update-value cur pwd parent)
    (when (< 0 new-total)
        (update-value (total-key prev) (+
                                        ;(get-value (total-key cur) pwd)
                                        new-total
                                        (get-value (total-key prev) parent))
                    parent)
        ;(format t "Added total of ~a to ~a~%" cur prev)
        )
    ;(format t "Total ~d assigned to ~a~%" (get-value (total-key prev) parent) prev)
    (update-value prev (get-value (total-key prev) parent) *totals*)
    (unless (equal "/" prev)
      (if final
          (update-prev-alist parent prev new-total final)
        (update-prev-alist parent prev 0)))))


(ql:quickload :cl-ppcre )

(defun load-file (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil)
          while line
          collect line)))

(defvar *file* nil) ;;procedure

(setf *file* (load-file "day7_input.txt")) ;; procedure
;(setf *file* (load-file "./test_input_day7.txt")) ;; procedure

;; (defvar pwd nil)
;; (setf pwd nil)
;; (defvar root nil)
;; (setf root nil)
;; (defvar prev nil)
;; (setf prev nil)
;; (defvar cur nil)
;; (setf cur nil)

(defun get-totals (file-list)
  (let (split-line
        filesize
        (new-files nil)
        (new-total 0))
    ;(format t "Begin File Read")
    (dolist (line file-list)
      ;(format t "File Line: ~a~%" line)
      (setf split-line (cl-ppcre:split :whitespace-char-class line))
      (cond
        ((equal "$" (first split-line))
         (cond
           ((equal "cd" (second split-line))
            (cond
              ((equal ".." (third split-line))
               ;(format t "Reached $ cd ..~%")
               ;(when new-files
                 (update-prev-alist pwd cur (get-value (total-key cur) pwd))
                 ;)
               (setf pwd (get-prev-from-cur cur))
               (setf cur (get-value "dir" pwd))
               ;; (unless new-files
               ;;   (update-prev-alist pwd cur (get-value (total-key cur) pwd)))
               (setf new-files nil)
               )
              ((equal "/" (third split-line))
               ;(format t "Reached $ cd /~%")
               (setf cur "/")
               (setf pwd (new-pwd cur)))
              ('t
               ;(format t "Reached $ cd ~a~%" (third split-line))
               ;(update-prev-alist pwd cur (get-value (total-key cur) pwd))
               (update-prev-alist pwd cur 0)
               (setf prev cur)
               (setf cur (new-dir prev (third split-line)))
               (setf pwd (new-pwd cur)))))
           ('t
            ; only other option is ls
            ;(format t "Reached $ ls~%")
            (setf new-files 't)
            (setf pwd (add-value (total-key cur) 0 pwd)))))
         ((equal "dir" (first split-line))
          ;(format t "Reached dir ~a~%" (second split-line))
          (setf pwd (add-value (new-dir cur (second split-line)) nil pwd)))
         ('t
          ; only other option is file
          ;(format t "Reached ~a ~a~%" (first split-line) (second split-line))
          (setf filesize (parse-integer (first split-line)))
          (setf pwd (add-value (second split-line) filesize pwd))
          (update-value (total-key cur) (+ filesize (get-value (total-key cur) pwd)) pwd)
          (update-value cur (get-value (total-key cur) pwd) *totals*)
          ;(format t "total ~d assigned to ~a~%" (get-value (total-key cur) pwd) cur)
          ))
    )
    ; finally, save final directory
    ; loop to push the changes
    (update-prev-alist pwd cur (get-value (total-key cur) pwd) 't)))

(get-totals *file*) ; procedure

(defun count-totals (totals limit)
  (let ((running-count 0))
    (dolist (next totals)
      (when (>= limit (cdr next))
        (setf running-count (+ running-count (cdr next)))))
    running-count))

(format t "Day7 Part1: ~d~%" (count-totals *totals* part1-limit))

(defun get-free-space (totals)
  (let* ((disk-size 70000000)
        (space-needed 30000000)
         (unused (- disk-size (get-value "/" totals)))
         (target (- space-needed unused))
         (next-size 0)
         (running-min disk-size))
    ;(format t "unused: ~d~%" unused)
    ;(format t "target: ~d~%" target)
    (dolist (next totals)
      (setf next-size (cdr next))
      (when (and
             (<= target next-size )
             (>= running-min next-size))
        (setf running-min next-size)))
    running-min))


(defun sum-alist (alist)
  (let ((running-total 0)
        (cur ""))
    (dolist (next alist)
      (cond
        ((listp (cdr next))
         (setf running-total
               (+ running-total
                  (sum-alist (cdr next)))))
        ((integerp (cdr next))
         (unless (position #\/ (car next))
           (setf running-total
               (+ running-total
                  (cdr next)))))
        ((equal "dir" (car next))
         (setf cur (cdr next)))))
    ;(format t "Total for ~a: ~d~%" cur running-total)
    (update-value cur running-total *totals*)
    running-total))

(sum-alist root) ;procedure
                 ;
(format t "Day7 Part2: ~d~%" (get-free-space *totals*))
