#!/usr/bin/env sbcl --script
(require "asdf")

(defun is_game_possible(game)
    (let ((tokens (remove-if (lambda (s) (string= "" s)) (uiop:split-string game :separator " ,;:"))))
            (assert (string= "Game" (pop tokens)))
            (pop tokens) ; Game#
            (loop while tokens do
            (let* ((val (parse-integer (pop tokens)))
                    (key (char (pop tokens) 0)))
                ; (format t "~a => ~a~%" key val)
                (cond
                    ((eql #\r key) (if (> val 12) (return-from is_game_possible nil)))
                    ((eql #\g key) (if (> val 13) (return-from is_game_possible nil)))
                    ((eql #\b key) (if (> val 14) (return-from is_game_possible nil))))))
            t))

(defun game_power(game)
    (let ((tokens (remove-if (lambda (s) (string= "" s)) (uiop:split-string game :separator " ,;:")))
          (r 0) (g 0) (b 0))
        (assert (string= "Game" (pop tokens)))
        (pop tokens) ; Game#
        (loop while tokens do
        (let* ((val (parse-integer (pop tokens)))
                (key (char (pop tokens) 0)))
            (cond
                ((eql #\r key) (if (> val r) (setq r val)))
                ((eql #\g key) (if (> val g) (setq g val)))
                ((eql #\b key) (if (> val b) (setq b val))))))
        (* r g b)))

(defun calculate_game_sum(filename)
    (with-open-file (file filename :direction :input)
        (let ((game_id  1) (game_sum 0))
            (loop for game = (read-line file nil) while game do
                (if (is_game_possible game) (incf game_sum game_id))
                (incf game_id))
            (format t "~a game sum is ~a~%" filename game_sum))))

(defun calculate_game_power_sum(filename)
    (with-open-file (file filename :direction :input)
        (let ((game_power_sum 0))
            (loop for game = (read-line file nil) while game do
                (incf game_power_sum (game_power game)))
            (format t "~a game power sum is ~a~%" filename game_power_sum))))

(calculate_game_sum "sample.txt")
(calculate_game_power_sum "sample.txt")

(calculate_game_sum "input.txt")
(calculate_game_power_sum "input.txt")