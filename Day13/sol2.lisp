#!/usr/bin/env sbcl --script
(require "asdf")

(defun check-symmetry(lidx ridx matrix offbits)
    (loop for off from 1
            for i = (- lidx off)
            for j = (+ ridx off)
            while (and (>= i 0) (<  j (length matrix))) do
        (incf offbits (logcount (logxor (nth i matrix) (nth j matrix))))
        (if (> offbits 1) ; at most 1 bit error 
            (return-from check-symmetry nil))
    )
    (eql offbits 1)
)

(defun find-symmetry(matrix)
    (loop for lidx from 0
          for ridx from 1
          for (lhs rhs) on matrix
          while rhs do
        (let ((offbits (logcount (logxor lhs rhs))))
            (if (<= offbits 1)
                (if (check-symmetry lidx ridx matrix offbits)
                    (return-from find-symmetry ridx)))
        )
    )
    0
)

(defun read-grid(filename)
    (with-open-file (file filename :direction :input)
        (let* ((matrix nil)
               (transp nil)
               (temp   nil)
               (total  0))

            (loop for line = (read-line file nil) while line do
                (cond
                    ((string= "" line)
                        (incf total (* (find-symmetry (reverse matrix)) 100))
                        (incf total (find-symmetry transp))
                        (setq matrix nil)
                        (setq transp nil))
                    (t
                        (setq temp (coerce line 'list))
                        (push 
                            (reduce (lambda(val sym) (+ (ash val 1) (if (eql sym #\#) 1 0))) temp :initial-value 0)
                            matrix)

                        (setq 
                            transp
                            (if (eql transp nil)
                                (mapcar (lambda(sym) (if (eql sym #\#) 1 0)) temp)
                                (mapcar #'+
                                    (mapcar (lambda(num) (ash num 1)) transp)
                                    (mapcar (lambda(sym) (if (eql sym #\#) 1 0)) temp))))
                    )
                )
            )
            total
        )
    )
)

(format t "sample.txt part2: ~a~%" (read-grid "sample.txt"))
(format t "input.txt part2: ~a~%" (read-grid "input.txt"))
