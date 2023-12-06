#!/usr/bin/env sbcl --dynamic-space-size 32768 --script
(require "asdf")

(defun parse-seeds(line)
    (map 'list (lambda (n) (parse-integer n))
        (cdr ; skip "seeds:"
            (remove-if (lambda (s) (string= "" s))
                (uiop:split-string line :separator " "))))
)

(defun parse-ranges(file)
    (sort
        (loop for line = (read-line file nil)
                    while (> (length line) 0)
                    collect (map 'list (lambda (n) (parse-integer n))
                                (uiop:split-string line :separator " ")))
        #'<
        :key #'second
    )
)

(defun parse-input(filename)
    (let ((seeds nil)
          (seed-to-soil nil)
          (soil-to-fertilizer nil)
          (fertilizer-to-water nil)
          (water-to-light nil)
          (light-to-temperature nil)
          (temperature-to-humidity nil)
          (humidity-to-location nil))
        (with-open-file (file filename :direction :input)
            (setq seeds (parse-seeds (read-line file nil)))
            (assert (string= (read-line file nil) ""))

            (assert (string= (read-line file nil) "seed-to-soil map:"))
            (setq seed-to-soil (parse-ranges file))

            (assert (string= (read-line file nil) "soil-to-fertilizer map:"))
            (setq soil-to-fertilizer (parse-ranges file))

            (assert (string= (read-line file nil) "fertilizer-to-water map:"))
            (setq fertilizer-to-water (parse-ranges file))

            (assert (string= (read-line file nil) "water-to-light map:"))
            (setq water-to-light (parse-ranges file))

            (assert (string= (read-line file nil) "light-to-temperature map:"))
            (setq light-to-temperature (parse-ranges file))

            (assert (string= (read-line file nil) "temperature-to-humidity map:"))
            (setq temperature-to-humidity (parse-ranges file))

            (assert (string= (read-line file nil) "humidity-to-location map:"))
            (setq humidity-to-location (parse-ranges file))

            (list seeds
                seed-to-soil
                soil-to-fertilizer
                fertilizer-to-water
                water-to-light
                light-to-temperature
                temperature-to-humidity
                humidity-to-location)
        )
    )
)

(defun seeds-to-lowest-location(ids maps)
    (let* ((aux nil)
           (src-start 0)
           (src-stop  0)
           (dst-start 0)
           (range-len 0))
        (loop for m in maps do
            (loop for r in m while ids do
                (setq dst-start (first  r))
                (setq src-start (second r))
                (setq range-len (third  r))

                ; ids preceding range
                (loop while (if ids (< (car ids) src-start)) do
                    (push (pop ids) aux))

                (setq src-stop (+ src-start range-len))

                ; ids in range
                (loop while (if ids
                    (and (>= (car ids) src-start) (< (car ids) src-stop))) do
                        (push (+ (- (pop ids) src-start) dst-start) aux))
            )
            (setq ids (concatenate 'list (reverse aux) ids))
            (setq ids (sort ids #'<))
            (setq aux nil)
        )
        (first ids)
    )
)

(defun expand-range(range-start range-length)
    (loop for i from range-start below (+ range-start range-length) collect i))

(format t "part1: ~a~%"
    (let* ((input (parse-input "input.txt")))
        (seeds-to-lowest-location (car input) (cdr input))
    )
)

(format t "part2: ~a~%"
    (let* ((input (parse-input "input.txt"))
           (aux nil)
           (lowest nil)
           (chunk nil)
           (seeds nil))
        (loop for (beg len) on (car input) by #'cddr do
            (loop for s from beg below (+ beg len) by 2000000 do
                (setq seeds (loop for i from s below (+ s 2000000) collect i))
                (loop while seeds do
                    (setq chunk nil)
                    (loop for i from 0 below 2000000 while seeds do
                        (push (pop seeds) chunk)
                    )
                    (setq chunk (reverse chunk))
                    (setq aux (seeds-to-lowest-location chunk (cdr input)))
                    (cond
                        ((not lowest)
                            (setq lowest aux))
                        ((< aux lowest)
                            (setq lowest aux))
                    )
                )
            )
        )
        lowest
    )
)
