#!/usr/bin/env sbcl --script
(require "asdf")

(defun parse-numbers(line)
    (map 'list
        (lambda (n) (parse-integer n))
            (cdr 
                (remove-if (lambda (s) (string= "" s))
                    (uiop:split-string line :separator " ")))))

(defun travel-distance(hold-duration race-duration)
    (* (- race-duration hold-duration) hold-duration))

(defun longest-hold(race-duration race-distance)
    (loop for hold-duration from (- race-duration 1) above 0 do
        (if (> (travel-distance hold-duration race-duration) race-distance)
            (return-from longest-hold hold-duration)))
)

(defun shortest-hold(race-duration race-distance)
    (loop for hold-duration from 1 below race-duration do
        (if (> (travel-distance hold-duration race-duration) race-distance)
            (return-from shortest-hold hold-duration)))
)

(defun calculate-race-hold-product(filename)
    (with-open-file (file filename :direction :input)
        (let* ((race-durations  (parse-numbers (read-line file nil)))
               (race-distances  (parse-numbers (read-line file nil)))
               (accumulator     1))
            (loop for dur in race-durations
                  for dis in race-distances
                  do
                    (setq accumulator
                        (* accumulator
                            (+ 1 (- (longest-hold dur dis) (shortest-hold dur dis)))))
            )
            accumulator
        )
    )
)

(format t "sample.txt part1: ~a~%" (calculate-race-hold-product "sample1.txt"))
(format t "input.txt part1: ~a~%"  (calculate-race-hold-product "input1.txt"))

; using modified files because not worth changing the parser
(format t "sample.txt part2: ~a~%" (calculate-race-hold-product "sample2.txt"))
(format t "input.txt part2: ~a~%"  (calculate-race-hold-product "input2.txt"))