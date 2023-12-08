#!/usr/bin/env sbcl --script
(require "asdf")

(defun make-step-guide(line)
    (let ((step-guide
            (loop for dir across line collect
                (cond ((eql dir #\L) 0)
                      ((eql dir #\R) 1)
                      (t (assert nil))))))
        (make-array (list (length step-guide)) :initial-contents step-guide)))

(defun parse-node-map(line)
    (list (subseq line 0 3) (subseq line 7 10) (subseq line 12 15)))

(defun calculate-distance(filename)
    (with-open-file (file filename :direction :input)
        (let* ((steps 0)
               (curr  "AAA") 
               (instr-arr (make-step-guide (read-line file nil)))
               (instr-len (array-dimension instr-arr 0))
               (_     (assert (string= "" (read-line file nil))))
               (nodes (make-hash-table :test 'equal))
               (aux  nil)
               (dir  nil)
               (idx  nil)
              )
            (loop for node-line = (read-line file nil) while node-line do
                (setq aux (parse-node-map node-line))
                (setf (gethash (car aux) nodes) (cdr aux)))
            (loop while (not (string= curr "ZZZ")) do
                (setq idx (mod steps instr-len))
                (setq dir (aref instr-arr idx))
                (setq curr (nth dir (gethash curr nodes)))
                (incf steps))
            steps)
    )
)

(format t "sample1.txt part1: ~a~%" (calculate-distance "sample1.txt"))
(format t "sample2.txt part1: ~a~%" (calculate-distance "sample2.txt"))
(format t "input.txt part1: ~a~%" (calculate-distance "input.txt"))