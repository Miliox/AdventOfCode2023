#!/usr/bin/env sbcl --script
(require "asdf")

(defun parse-hand(line)
    (let* ((tokens (uiop:split-string line :separator " "))
           (cards  (loop for card across (first tokens) collect
                        (cond ((eql #\2 card) 0)
                              ((eql #\3 card) 1)
                              ((eql #\4 card) 2)
                              ((eql #\5 card) 3)
                              ((eql #\6 card) 4)
                              ((eql #\7 card) 5)
                              ((eql #\8 card) 6)
                              ((eql #\9 card) 7)
                              ((eql #\T card) 8)
                              ((eql #\J card) 9)
                              ((eql #\Q card) 10)
                              ((eql #\K card) 11)
                              ((eql #\A card) 12))))
           (kind   (calculate-kind-rank cards))
           (bid    (parse-integer (second tokens)))
          )
        (list kind cards bid (first tokens))))

(defun calculate-kind-rank(cards)
    (setq cards (sort (copy-list cards) #'<))
    (let* ((card-count 0)
           (prev-card  nil)
           (card-count-list nil)
          )
        (loop for card in cards do
            (cond
                ((or (eql card prev-card) (not prev-card))
                    (incf card-count))
                (t
                    (push card-count card-count-list)
                    (setq card-count 1)
                )
            )
            (setq prev-card card)
        )
        (push card-count card-count-list)
        (setq card-count-list (sort card-count-list #'>))
        (cond
            ((equal card-count-list (list 5))
                6) ; five of a kind
            ((equal card-count-list (list 4 1))
                5) ; four of a kind
            ((equal card-count-list (list 3 2))
                4) ; full house
            ((equal card-count-list (list 3 1 1))
                3) ; three of a kind
            ((equal card-count-list (list 2 2 1))
                2) ; two pair
            ((equal card-count-list (list 2 1 1 1))
                1) ; one pair
            ((equal card-count-list (list 1 1 1 1 1))
                0) ; high card
        )
    )
)

(defun hand-comparator(lhs rhs)
    (let ((lhs-type (first lhs))
          (rhs-type (first rhs)))
        (if (> lhs-type rhs-type) (return-from hand-comparator nil))
        (if (< lhs-type rhs-type) (return-from hand-comparator t))
    )
    (let ((lhs-cards (second lhs))
          (rhs-cards (second rhs)))
        (loop for lhs-card in lhs-cards 
              for rhs-card in rhs-cards
            do
                (if (> lhs-card rhs-card) (return-from hand-comparator nil))
                (if (< lhs-card rhs-card) (return-from hand-comparator t))
        )
    )
    (let ((lhs-bid (third lhs))
          (rhs-bid (third rhs)))
        (if (> lhs-bid rhs-bid) (return-from hand-comparator nil))
        (if (< lhs-bid rhs-bid) (return-from hand-comparator t))
    )
    nil
)

(defun calculate-total-winnings(filename)
    (with-open-file (file filename :direction :input)
        (let* ((sorted-hands
                    (sort
                        (loop for line = (read-line file nil)
                            while line
                            collect (parse-hand line))
                        #'hand-comparator))
               (index 1)
               (total 0))
            (loop for hand in sorted-hands do
                (incf total (* index (third hand)))
                (incf index))
            total)
    )
)

(format t "sample.txt part1: ~a~%" (calculate-total-winnings "sample.txt"))
(format t "input.txt part1: ~a~%" (calculate-total-winnings "input.txt"))