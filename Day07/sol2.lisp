#!/usr/bin/env sbcl --script
(require "asdf")

(defun parse-hand(line)
    (let* ((tokens (uiop:split-string line :separator " "))
           (cards  (loop for card across (first tokens) collect
                        (cond ((eql #\J card) 0)
                              ((eql #\2 card) 1)
                              ((eql #\3 card) 2)
                              ((eql #\4 card) 3)
                              ((eql #\5 card) 4)
                              ((eql #\6 card) 5)
                              ((eql #\7 card) 6)
                              ((eql #\8 card) 7)
                              ((eql #\9 card) 8)
                              ((eql #\T card) 9)
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
           (joker-count 0)
           (prev-card  nil)
           (card-count-list nil))
        (loop for card in cards do
            (cond
                ((or (eql card prev-card)
                     (not prev-card))
                    (if (eql card 0)
                        (incf joker-count)
                        (incf card-count)
                    )
                )
                (t
                    (if (not (eql prev-card 0))
                        (push card-count card-count-list))
                    (setq card-count 1)
                )
            )
            (setq prev-card card)
        )
        (if (not (eql prev-card 0))
            (push card-count card-count-list))
        
        (setq card-count-list (sort card-count-list #'>))
        (if (not card-count-list) ; all cards were joker
            (push joker-count card-count-list)
            (push (+ joker-count (pop card-count-list)) card-count-list))

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
                ;(format t "~a~%" hand)
                (incf total (* index (third hand)))
                (incf index))
            total)
    )
)

(format t "sample.txt part2: ~a~%" (calculate-total-winnings "sample.txt"))
(format t "input.txt part2: ~a~%" (calculate-total-winnings "input.txt"))