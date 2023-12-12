#!/usr/bin/env sbcl --script
(require "asdf")

(defun load-galaxies(filename)
    (with-open-file (file filename :direction :input)
        (let ((galaxies nil))
            (loop for row = (read-line file nil) while row
                  for i from 0 do
                (loop for c across row
                      for j from 0
                      when (eq c #\#) do (push (list j i) galaxies))
            )
            (reverse galaxies))))

(defun apply-expansion(galaxies)
    (let ((row-table (make-hash-table))
          (col-table (make-hash-table)))
        ; calculate new horizontal values with expansion
        (loop for e in (remove-duplicates (sort (map 'list #'first galaxies) #'<))
              for n from 0 do (setf (gethash e col-table) (+ e (- e n))))
        ; calculate new vertical values with expansion
        (loop for e in (remove-duplicates (sort (map 'list #'second galaxies) #'<))
              for n from 0 do (setf (gethash e row-table) (+ e (- e n))))
        ; map galaxy coordinate to new values
        (map 'list (lambda (g) (list (gethash (first  g) col-table)
                                     (gethash (second g) row-table))) galaxies)
    )
)

(defun all-pairs(items)
    (let ((pairs nil))
        (loop for a = (car items) while items do
            (loop for b in (cdr items) do
                (push (list a b) pairs))
            (setq items (cdr items)))
        pairs))

(defun distance(a b)
    (let ((aj (first  a))
          (ai (second a))
          (bj (first  b))
          (bi (second b))
          (steps 0))
        (loop while (or (not (eql bj aj)) (not (eql bi ai))) do
            (cond 
                ((eql bj aj)
                    (incf steps (abs (- bi ai)))
                    (setq ai bi)
                )
                ((eql bi ai)
                    (incf steps (abs (- bj aj)))
                    (setq aj bj)
                )
                ((<= (abs (- bj aj)) (abs (- bi ai)))
                    (incf steps (abs (- bj aj)))
                    (incf aj (* (abs (- bj aj)) (if (>= bj aj) 1 -1)))
                )
                (t
                    (incf steps (abs (- bi ai)))
                    (incf ai (* (abs (- bi ai)) (if (>= bi ai) 1 -1)))
                )
            )
        )
        steps
    )
)

(format t "sample.txt part1: ~a~%"
    (reduce (lambda (total pair) (+ total (distance (first pair) (second pair))))
        (all-pairs
            (apply-expansion
                (load-galaxies "sample.txt")))
        :initial-value 0))

(format t "input.txt part1: ~a~%"
    (reduce (lambda (total pair) (+ total (distance (first pair) (second pair))))
        (all-pairs
            (apply-expansion
                (load-galaxies "input.txt")))
        :initial-value 0))