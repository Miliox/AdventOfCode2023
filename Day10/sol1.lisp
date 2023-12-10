#!/usr/bin/env sbcl --script
(require "asdf")

(defun load-diagram(filename)
    (with-open-file (file filename :direction :input)
        (let* ((diagram
                    (loop for line = (read-line file nil) while line
                        collect (loop for letter across line collect letter)))
               (rows (length diagram))
               (cols (length (car diagram))))
            (make-array (list rows cols) :initial-contents diagram))))

(defun find-start(diagram)
    (let ((rows (array-dimension diagram 0))
          (cols (array-dimension diagram 1)))
        (loop for i from 0 below rows do
            (loop for j from 0 below cols do
                (if (eql #\S (aref diagram i j))
                    (return-from find-start (list i j)))))))

(defun start-symbol(pos diagram)
    (let* ((n (mapcar '- pos '(1 0)))
           (nt (if (>= (first n) 0)
                (aref diagram (first n) (second n))
                #\.))
           (nc (member nt '(#\| #\7 #\F)))
           (s (mapcar '+ pos '(1 0)))
           (st (if (< (first s) (array-dimension diagram 0))
                (aref diagram (first s) (second s))
                #\.))
           (sc (member st '(#\| \#J #\L)))
           (w (mapcar '- pos '(0 1)))
           (wt (if (>= (second w) 0)
                 (aref diagram (first w) (second w))
                 #\.))
           (wc (member wt '(#\- #\L #\F)))
           (e (mapcar '+ pos '(0 1)))
           (et (if (< (second e) (array-dimension diagram 0))
                (aref diagram (first e) (second e))
                #\.))
           (ec (member et '(#\- #\7 #\J))))
        (cond
            ((and nc sc) #\|)
            ((and wc ec) #\-)
            ((and nc wc) #\J)
            ((and nc ec) #\L)
            ((and sc wc) #\7)
            ((and sc ec) #\F)
            (t
                (format t "adjacent char ~a ~a ~a ~a~%" nt st wt et)
                (format t "adjacent conn ~a ~a ~a ~a~%" nc sc wc ec)
                (assert nil))
        )
    )
)

(defun find-adjacents(pos diagram)
    (let* ((n (mapcar '- pos (list 1 0)))
           (s (mapcar '+ pos (list 1 0)))
           (w (mapcar '- pos (list 0 1)))
           (e (mapcar '+ pos (list 0 1)))
           (tile (aref diagram (first pos) (second pos))))
        (cond 
            ((eql tile #\|) (list n s))
            ((eql tile #\-) (list w e))
            ((eql tile #\L) (list n e))
            ((eql tile #\J) (list n w))
            ((eql tile #\7) (list w s))
            ((eql tile #\F) (list e s))
            ((eql tile #\.) nil)
            (t 
                (format t "invalid tile ~a~%" tile)
                (assert nil))
        )
    )
)

(defun traverse(filename)
    (let* ((diagram (load-diagram filename))
           (rows  (array-dimension diagram 0))
           (cols  (array-dimension diagram 1))
           (start (find-start diagram))
           (step nil)
           (curr-steps nil)
           (next-steps (list (list start 0)))
           (visited nil)
           (distance 0))

        ; replace start symbol
        (setf (aref diagram (first start) (second start)) (start-symbol start diagram))

        (loop while next-steps do
            (setq curr-steps next-steps)
            (setq next-steps nil)

            (loop while curr-steps do
                (setq step (pop curr-steps))
                (cond
                    ((not (find (first step) visited :test #'equal))
                        (push (first step) visited) ; visit
                        (loop for adj in (find-adjacents (first step) diagram) do
                            (if (not (find adj visited :test #'equal))
                                (push (list adj (+ (second step) 1)) next-steps))))
                    (t
                        (if (< distance (second step)) (setq distance (second step)))
                    )
                )
            )
        )
        distance
    )
)


(format t "sample1.txt part1: ~a~%" (traverse "sample1.txt"))
(format t "sample2.txt part1: ~a~%" (traverse "sample2.txt"))
(format t "input.txt part1: ~a~%" (traverse "input.txt"))