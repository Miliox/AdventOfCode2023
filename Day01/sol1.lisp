(defvar digits)
(defvar str)
(defvar num)
(defvar total 0)
(with-open-file (file "input.txt" :direction :input)
    (loop for line = (read-line file nil)
          while line
          do (setq digits (remove-if-not #'digit-char-p  line))
             (setq str (format nil "~a~a" (char digits 0) (char digits (- (length digits) 1))))
             (setq num (parse-integer str))
             (setq total (+ num total))
    )
)
(format T "~a~%" total)