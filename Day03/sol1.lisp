#!/usr/bin/env sbcl --script
(require "asdf")

(defun load_schema(filename)
    (let ((buffer nil))
        (with-open-file (file filename :direction :input)
        (loop for line = (read-line file nil) while line do
            (push (coerce line 'list) buffer)))
        (make-array
            (list (length buffer) (length (first buffer)))
            :initial-contents (reverse buffer))))



(let* ((schema (load_schema "input.txt"))
       (ymax (- (array-dimension schema 0) 1))
       (xmax (- (array-dimension schema 1) 1))
       (prev #\.)
       (curr #\.)
       (acc 0)
       (val nil)
       (sum 0)
       (reading nil)
       (acc_near nil)
       (prev_near nil)
       (curr_near nil))
    ;(format t "~a~%" schema)
    (loop for y from 0 to ymax do
        (loop for x from 0 to xmax do
            (setq curr (aref schema y x))
            (setq val  (digit-char-p curr))

            ;(format t "~a ~a => ~a ~a~%" x y curr val)
            (setq curr_near (or
                (if (eql y 0) nil
                    (not (eql #\. (aref schema (- y 1) x))))
                (if (eql y ymax) nil
                    (not (eql #\. (aref schema (+ y 1) x))))
            ))

            (cond
                (val
                    (setq acc (+ (* acc 10) val))
                    (if (not reading)
                        (setq acc_near (or prev_near (not (eql prev #\.)))))
                    (setq acc_near (or acc_near curr_near))
                    (setq reading t)
                )
                (reading
                    (cond 
                        ((or acc_near curr_near (not (eql curr #\.)))
                            ;(format t "~a~%" acc)
                            (incf sum acc)
                        ))
                    (setq acc 0)
                    (setq acc_near nil)
                    (setq reading nil)
                )
                (t
                    (setq acc_near (or (not (eql #\. curr)) curr_near))
                )
            )

            (cond
                ((and reading (eql x xmax))
                    (cond
                        (acc_near
                            ;(format t "~a~%" acc)
                            (incf sum acc)))
                    (setq acc 0)
                    (setq reading nil)
                    (setq prev #\.)
                    (setq prev_near nil)
                    (setq acc_near nil)
                )
                (t
                    (setq prev curr)
                    (setq prev_near curr_near)
                )
            )
        )
    )
    (format t "~a~%" sum)
)
