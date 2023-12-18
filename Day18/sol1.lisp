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

(defconstant up-dir    (list -1 +0))
(defconstant down-dir  (list +1 +0))
(defconstant left-dir  (list +0 -1))
(defconstant right-dir (list +0 +1))

(defun decode-direction(letter)
    (cond ((string= letter "U") up-dir)
          ((string= letter "D") down-dir)
          ((string= letter "L") left-dir)
          ((string= letter "R") right-dir)
          (t (assert nil))))

(defun load-plan(filename)
    (with-open-file (file filename :direction :input)
        (loop for instruction = (read-line file nil)
              while instruction
              for tokens = (uiop:split-string instruction :separator " ")
              for direction = (decode-direction (first tokens))
              for ammount = (parse-integer (second tokens))
              for color   = (third tokens)
            collect
                (list direction ammount color))))

(defun find-start(row_min col_min col_max coords)
    (loop for col from col_min upto col_max do
        (if (and (gethash (list row_min col) coords)
                 (not (gethash (list (+ row_min 1) col) coords)))
            (return-from find-start (list (+ row_min 1) col))
        )
    )
)

(format t "input.txt ~a~%" 
    (let* ((plan (load-plan "input.txt"))
           (coords (make-hash-table :test 'equal))
           (pos (list 0 0))
           (row_min 0) (row_max 0)
           (col_min 0) (col_max 0)
           (walks nil)
          )

        (setf (gethash pos coords) 1);"(#000000)")

        (loop for instruction in plan
              for direction = (first instruction)
              for ammount = (second instruction)
              ;for color = (third instruction)
            do (loop for step from 0 below ammount do
                    (setq pos (mapcar '+ pos direction))
                    (setf (gethash pos coords) 1)

                    (if (< (first pos) row_min)
                        (setq row_min (first pos)))

                    (if (> (first pos) row_max)
                        (setq row_max (first pos)))

                    (if (< (second pos) col_min)
                        (setq col_min (second pos)))

                    (if (> (second pos) col_max)
                        (setq col_max (second pos)))
               ))

        ; (format t "~a,~a ~a,~a~%" row_min col_min row_max col_max)
        (push (find-start row_min col_min col_max coords) walks)
        (loop while walks
              for pos = (pop walks)
              :if (not (gethash pos coords)) :do
                  (setf (gethash pos coords) 1)
                  (push (mapcar '+ pos up-dir) walks)
                  (push (mapcar '+ pos down-dir) walks)
                  (push (mapcar '+ pos left-dir) walks)
                  (push (mapcar '+ pos right-dir) walks)
              :end 
        )
        coords
    )
)
