#!/usr/bin/env sbcl --script
(require "asdf")

(defun score-scratchcard(scratchcard)
    (let* ((payload
                (string-trim " "
                    (nth 1 (uiop:split-string scratchcard :separator ":"))))
           (sections
                (map 'list (lambda (section) (string-trim " " section)) 
                    (uiop:split-string payload :separator "|")))
           (winning_numbers
                (map 'list (lambda (n) (parse-integer n))
                    (remove-if (lambda (s) (string= "" s))
                        (uiop:split-string (nth 0 sections) :separator " "))))
           (owning_numbers
                (map 'list (lambda (n) (parse-integer n))
                    (remove-if (lambda (s) (string= "" s))
                        (uiop:split-string (nth 1 sections) :separator " "))))
           (matching_numbers
                (intersection winning_numbers owning_numbers))
           (matching_count
                (length matching_numbers)))
        (if (> matching_count 0)
                (expt 2 (- matching_count 1))
                0)))

(defun calculate-scratchcard-total-score(filename)
    (with-open-file (file filename :direction :input)
        (let ((total_score 0))
            (loop for scratchcard = (read-line file nil) while scratchcard do
                ;(format t "~a~%" (score-scratchcard scratchcard))
                (incf total_score (score-scratchcard scratchcard)))
            total_score
        )
    )
)

(format t "~a~%" (calculate-scratchcard-total-score "sample.txt"))
(format t "~a~%" (calculate-scratchcard-total-score "input.txt"))