#!/usr/bin/env sbcl --script
(require "asdf")

(defun parse-summary(summary)
    (map 'list (lambda (s) (parse-integer s)) (uiop:split-string summary :separator ",")))

(defun parse-springs(springs)
    (setq springs (reduce
        (lambda (aggr curr-tile)
            (let* ((head (car aggr))
                    (tail (cdr aggr))
                    (prev-tile (first  head))
                    (prev-acc  (second head)))
                (cond
                    ((eql curr-tile prev-tile)
                        (append (list (list curr-tile (+ prev-acc 1))) tail))
                    (t
                        (append (list (list curr-tile 1)) aggr))))
        )
        (loop for c across springs collect c)
        :initial-value nil))
    (if (eql #\. (car (car springs))) (pop springs))
    (setq springs (reverse springs))
    (if (eql #\. (car (car springs))) (pop springs))
    springs
)

(defun trim-limits(springs summary)
    (loop for spring = (first springs)
          for tile = (first spring)
          for tile-count = (second spring)
          for summ-count = (first summary)
          while (or (eql #\. tile) (and (eql #\# tile) (eql tile-count summ-count)))
          do
          (pop springs)
          (if (eql #\# tile) (pop summary))
        )
    (setq springs (reverse springs))
    (setq summary (reverse summary))
    (loop for spring = (first springs)
          for tile = (first spring)
          for tile-count = (second spring)
          for summ-count = (first summary)
          while (or (eql #\. tile) (and (eql #\# tile) (eql tile-count summ-count)))
          do
          (pop springs)
          (if (eql #\# tile) (pop summary))
        )
    (setq springs (reverse springs))
    (setq summary (reverse summary))
    (list springs summary)
)

(defun find-possible-arrangements(filename)
    (with-open-file (file filename :direction :input)
        (loop for input = (read-line file nil) while input collect
            (apply 'trim-limits
                (apply 
                    (lambda (springs summary)
                        (list
                            (parse-springs springs)
                            (parse-summary summary)
                        )
                    )
                    (uiop:split-string input :separator " ")
                )
            )
        )
    )
)

(format t "sample.txt part1: ~a~%" (find-possible-arrangements "sample.txt"))
