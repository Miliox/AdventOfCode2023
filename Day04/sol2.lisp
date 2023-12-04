#!/usr/bin/env sbcl --script
(require "asdf")

(defun zip(list1 list2)
    (let ((item1 (pop list1))
          (item2 (pop list2))
          (list3 nil))
        (loop while (or item1 item2) do
            (push (+ (if item1 item1 0) (if item2 item2 0)) list3)
            (setq item1 (pop list1))
            (setq item2 (pop list2))
        )
    (reverse list3))
)

(defun score-scratchcard(scratchcard)
    (let* ((payload
                (string-trim " "
                    (nth 1 (uiop:split-string scratchcard :separator ":"))))
           (sections
                (map 'list (lambda (section) (string-trim " " section)) 
                    (uiop:split-string payload :separator "|")))
           (winning_numbers
                (map 'list (lambda (n) (parse-integer n))
                    (remove-if (lambda (s) (string= "" s))
                        (uiop:split-string (nth 0 sections) :separator " "))))
           (owning_numbers
                (map 'list (lambda (n) (parse-integer n))
                    (remove-if (lambda (s) (string= "" s))
                        (uiop:split-string (nth 1 sections) :separator " "))))
           (matching_numbers
                (intersection winning_numbers owning_numbers))
           (matching_count
                (length matching_numbers)))
        (make-list matching_count :initial-element 1)))

(defun calculate-scratchcard-total-score(filename)
    (with-open-file (file filename :direction :input)
        (let ((total_cards 0)
              (card_copies 0)
              (extra_card_sequence nil)
              (acquired_extra_card_sequence nil)
             )
            (loop for scratchcard = (read-line file nil) while scratchcard do
                (setq card_copies (pop extra_card_sequence))
                (setq card_copies (+ 1 (if card_copies card_copies 0)))
                (incf total_cards card_copies)

                (setq acquired_extra_card_sequence
                    (map 'list (lambda (i) (* i card_copies))
                        (score-scratchcard scratchcard)))

                (setq extra_card_sequence (zip extra_card_sequence acquired_extra_card_sequence))
            )
            total_cards
        )
    )
)

(format t "~a~%" (calculate-scratchcard-total-score "sample.txt"))
(format t "~a~%" (calculate-scratchcard-total-score "input.txt"))