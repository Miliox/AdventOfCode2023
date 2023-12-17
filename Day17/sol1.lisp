#!/usr/bin/env sbcl --script
(require "asdf")

; TODO: figure out how use self install packages
(push "../../alexandria/" asdf:*central-registry*)
(push "../../bordeaux-threads/" asdf:*central-registry*)
(push "../../global-vars/" asdf:*central-registry*)
(push "../../lparallel/" asdf:*central-registry*)
(push "../../trivial-features/" asdf:*central-registry*)
(push "../../trivial-garbage/" asdf:*central-registry*)
(require "lparallel")

(push "../../priority-queue/" asdf:*central-registry*)
(require "priority-queue")

(defun load-grid(filename)
    (with-open-file (file filename :direction :input)
        (let* ((grid (loop for row = (read-line file nil) while row collect
                        (loop for col across row collect (- (char-code col) (char-code #\0)))))
               (row_len (length grid))
               (col_len (length (first grid))))
            (make-array (list row_len col_len) :initial-contents grid))))

(defconstant updir    (list -1 +0))
(defconstant downdir  (list +1 +0))
(defconstant leftdir  (list +0 -1))
(defconstant rightdir (list +0 +1))

(defun inbounds(pos row_max col_max)
    (and
        (>= (first  pos) 0)
        (>= (second pos) 0)
        (<=  (first  pos) row_max)
        (<=  (second pos) col_max))
)

(defun pathfinder(grid)
    (let* ((origin (list 0 0))
           (row_max (- (array-dimension grid 0) 1))
           (col_max (- (array-dimension grid 1) 1))
           (goal  (list row_max col_max))
           (paths (priority-queue:make-pqueue #'<))
           (visited nil)
        )
        ; start
        (priority-queue:pqueue-push
            (list
                origin ; posiiton
                (* -1 (aref grid (first origin) (second origin))) ; heat-lost
                (list 0 0) ; direction
                0) ; same-direction-counter
            0
            paths)
        
        (loop while (not (priority-queue:pqueue-empty-p paths))
            for path = (priority-queue:pqueue-pop paths)
            for position = (pop path)
            for heat-lost = (+ (pop path) (aref grid (first position) (second position)))
            for direction = (pop path)
            for dir-count = (pop path)
            for adj-up    = (mapcar '+ position updir)
            for adj-down  = (mapcar '+ position downdir)
            for adj-left  = (mapcar '+ position leftdir)
            for adj-right = (mapcar '+ position rightdir)
            if (<= dir-count 3) do

            (if (equal position goal)
                (return-from pathfinder heat-lost)
            )

            (setq visited (adjoin position visited))

            (if (and (inbounds adj-up row_max col_max)
                     (not (find adj-up visited :test 'equal)))
                (priority-queue:pqueue-push
                    (list adj-up heat-lost updir (if (equal updir direction) (+ dir-count 1) 1))
                    (+ heat-lost (aref grid (first adj-up) (second adj-up)))
                    paths)
            )

            (if (and (inbounds adj-down row_max col_max)
                     (not (find adj-down visited :test 'equal)))
                (priority-queue:pqueue-push
                    (list adj-down heat-lost downdir (if (equal downdir direction) (+ dir-count 1) 1))
                    (+ heat-lost (aref grid (first adj-down) (second adj-down)))
                    paths)
            )

            (if (and (inbounds adj-left row_max col_max)
                     (not (find adj-left visited :test 'equal)))
                (priority-queue:pqueue-push
                    (list adj-left heat-lost leftdir (if (equal leftdir direction) (+ dir-count 1) 1))
                    (+ heat-lost (aref grid (first adj-left) (second adj-left)))
                    paths)
            )

            (if (and (inbounds adj-right row_max col_max)
                     (not (find adj-right visited :test 'equal)))
                (priority-queue:pqueue-push
                    (list adj-right heat-lost rightdir (if (equal rightdir direction) (+ dir-count 1) 1))
                    (+ heat-lost (aref grid (first adj-right) (second adj-right)))
                    paths)
            )
        )
    )
)

(format t "sample.txt: ~a~%"
    (pathfinder
        (load-grid "sample.txt")
    )
)