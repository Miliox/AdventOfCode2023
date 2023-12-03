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

(defun find_number_at(schema init_x init_y xmax ymax)
    (if (< init_x 0) (return-from find_number_at nil))
    (if (< init_y 0) (return-from find_number_at nil))
    (if (> init_x xmax) (return-from find_number_at nil))
    (if (> init_y ymax) (return-from find_number_at nil))
    (if (not (digit-char-p (aref schema init_x init_y))) (return-from find_number_at nil))

    (let ((accumulator 0)
          (value 0)
          (start_y 0))
        (loop for y from init_y downto 0 while (digit-char-p (aref schema init_x y)) do (setq start_y y))
        (loop for y from start_y upto ymax while (setq value (digit-char-p (aref schema init_x y))) do
            (setq accumulator (+ value (* accumulator 10)))
        )
        accumulator
    )
)

(defun find_gear_ratio(schema gear_x gear_y xmax ymax)
    (let* ((up (find_number_at schema (- gear_x 1) gear_y xmax ymax))
           (down (find_number_at schema (+ gear_x 1) gear_y xmax ymax))
           (left (find_number_at schema gear_x (- gear_y 1) xmax ymax))
           (right (find_number_at schema gear_x (+ gear_y 1) xmax ymax))
           (part_nums (list left right up down)))

          (push (if (not up) (find_number_at schema (- gear_x 1) (- gear_y 1) xmax ymax)) part_nums)
          (push (if (not up) (find_number_at schema (- gear_x 1) (+ gear_y 1) xmax ymax)) part_nums)
          (push (if (not down) (find_number_at schema (+ gear_x 1) (- gear_y 1) xmax ymax)) part_nums)
          (push (if (not down) (find_number_at schema (+ gear_x 1) (+ gear_y 1) xmax ymax)) part_nums)

          (setq part_nums (remove nil part_nums))
          (if (= (length part_nums) 2) (* (pop part_nums) (pop part_nums)) 0)
    )
)

(let* ((schema (load_schema "input.txt"))
       (ymax (- (array-dimension schema 0) 1))
       (xmax (- (array-dimension schema 1) 1))
       (curr #\.)
       (sum 0))
    ;(format t "~a~%" schema)
    (loop for x from 0 to xmax do
        (loop for y from 0 to ymax do
            (setq curr (aref schema x y))
            (if (eql #\* curr)
                (incf sum (find_gear_ratio schema x y xmax ymax))
            )))
    (format t "~a~%" sum)
)
