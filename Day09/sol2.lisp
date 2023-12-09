#!/usr/bin/env sbcl --script
(require "asdf")

(defun difference(numbers)
    (loop for (n m) on numbers while m collect (- m n)))

(defun parse-numbers(line)
    (map 'list (lambda (n) (parse-integer n))
        (remove-if (lambda (s) (string= "" s))
            (uiop:split-string line :separator " "))))

(defun calculate-sum(filename)
    (with-open-file (file filename :direction :input)
        (let ((initial nil)
              (buffer  nil)
              (prev    0)
              (total   0))
            (loop for line = (read-line file nil) while line do
                ; read numbers
                (setq initial (parse-numbers line))
                (setq buffer  initial)

                ; calculate differences
                (setq buffer (loop while (notevery (lambda (n) (eql n 0)) buffer) collect (setq buffer (difference buffer))))

                ; pick first values
                (setq buffer (loop for l in buffer collect (car l)))

                ; extrapolate the previous sequence value from extrapolated differences
                (setq prev 0)
                (setq buffer (loop for n in (reverse buffer) collect (setq prev (- n prev))))

                ; sum the extrapolated value
                (incf total (- (car initial) (car (last buffer))))
            )
            total)))

(format t "sample.txt part2: ~a~%" (calculate-sum "sample.txt"))
(format t "input.txt part2: ~a~%" (calculate-sum "input.txt"))