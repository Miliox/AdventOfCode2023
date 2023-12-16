#!/usr/bin/env sbcl --script
(require "asdf")

(defun roll-rocks(column)
    (let ((result nil)
          (rock-buffer nil)
          (void-buffer nil)
          (tile   nil))
        (loop while column do
            (setq tile (pop column))
            (cond
                ((eql tile #\#)
                    (setq result (append (list tile) rock-buffer void-buffer result))
                    (setq rock-buffer nil)
                    (setq void-buffer nil)
                )
                ((eql tile #\.)
                    (push tile void-buffer))
                ((eql tile #\O)
                    (push tile rock-buffer))
                (t (assert nil))
            )
        )
        (reverse (append rock-buffer void-buffer result))
    )
)

(defun compute-load(columns)
    (let ((total 0))
        (loop for col in columns do
            (loop for i from 1
                  for c in col do
                (if (eql c #\O) (incf total i))))
        total
    )
)

(defun read-grid(filename)
    (with-open-file (file filename :direction :input)
        (let ((cols (map 'list (lambda (a) (list a)) (coerce (read-line file nil) 'list))))
            (loop for line = (read-line file nil) while line do
                (setq cols
                    (mapcar (lambda (ch col) (cons ch col)) (coerce line 'list) cols)))
            (compute-load (map 'list 'roll-rocks cols))
        )
    )
)

(format t "sample.txt part1: ~a~%" (read-grid "sample.txt"))
(format t "input.txt part1: ~a~%" (read-grid "input.txt"))
