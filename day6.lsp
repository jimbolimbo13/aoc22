;; Day6

(defun load-file (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil)
          while line
          collect line)))

(defvar *file* nil) ;;procedure

(setf *file* (load-file "day6_input.txt")) ;; procedure
;(setf *file* (load-file "./test_input_day6.txt")) ;; procedure

(defun make-set (elements)
  (let (out-set)
    (dolist (next elements)
      (setf out-set (adjoin next out-set)))
    out-set))

(defun find-signal (datastream marker-len)
  ; expects datastream to be alist of characters
    (loop for ix from marker-len to (length datastream) do
            (if (equal marker-len (length (make-set (subseq datastream
                                                    (- ix marker-len)
                                                    ix))))
                (return-from find-signal ix))))

(format t "Day6 Part1: ~d~%" (find-signal (coerce (car *file*) 'list) 4))
(format t "Day6 Part2: ~d~%" (find-signal (coerce (car *file*) 'list) 14))
