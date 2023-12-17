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

(defun parse(filename)
    (with-open-file (file filename :direction :input)
        (let* ((row_max 0)
               (col_max 0)
               (objects 
                    (apply 'append
                        (loop for row_idx from 0
                            for row_val = (read-line file nil)
                            while row_val
                            :if (> row_idx row_max)
                                :do (setq row_max row_idx)
                            :end
                            collect (loop for col_idx from 0
                                        for col_val across row_val
                                        :if (> col_idx col_max)
                                            :do (setq col_max col_idx)
                                        :end
                                        :if (not (eql col_val #\.))
                                            :collect (list row_idx col_idx col_val) :into elements
                                        :end
                                        :finally (return elements))
                        )
                    )
               )
               (obj_table (make-hash-table :test 'equal))
            )
            (loop for obj in objects
                  for i = (first  obj)
                  for j = (second obj)
                  for v = (third  obj)
                do
                     (setf (gethash (list i j) obj_table) v)
            )
            (list (+ row_max 1) (+ col_max 1) obj_table)
        )
    )
)

(defun inbounds(pos row_len col_len)
    (and
        (>= (first pos) 0)
        (>= (second pos) 0)
        (<  (first  pos) row_len)
        (<  (second pos) col_len))
)

(defconstant origin   (list +0 +0))
(defconstant updir    (list -1 +0))
(defconstant downdir  (list +1 +0))
(defconstant leftdir  (list +0 -1))
(defconstant rightdir (list +0 +1))

(defun traverse-beam(start_pos start_dir row_len col_len objects)
    (let ((visited (make-hash-table :test 'equal))
          (walks   (list (list start_pos start_dir))))
        (loop while walks
              for walk = (pop walks)
              for pos = (pop walk)
              for dir = (pop walk)
              for obj = (gethash pos objects)
              for vis = (gethash pos visited)
            :if (not (find dir vis :test 'equal)) :do
                (setf (gethash pos visited) (push dir vis))

                (cond
                    ((or (eql obj nil) ; vacant
                         (and (eql obj #\|) (or (equal dir updir)   (equal dir downdir)))
                         (and (eql obj #\-) (or (equal dir leftdir) (equal dir rightdir))))
                        (let ((next_pos (mapcar '+ pos dir)))
                            ; bound check
                            (if (inbounds next_pos row_len col_len)
                                (push (list next_pos dir) walks)
                            )
                        ))
                    ((and (eql obj #\-) (or (equal dir updir) (equal dir downdir)))
                        (let ((lpos (mapcar '+ pos leftdir))
                              (rpos (mapcar '+ pos rightdir)))
                            (if (inbounds lpos row_len col_len)
                                (push (list lpos leftdir) walks)
                            )

                            (if (inbounds rpos row_len col_len)
                                (push (list rpos rightdir) walks)
                            )
                        ))
                    ((and (eql obj #\|) (or (equal dir leftdir) (equal dir rightdir)))
                        (let ((upos (mapcar '+ pos updir))
                              (dpos (mapcar '+ pos downdir)))
                            (if (inbounds upos row_len col_len)
                                (push (list upos updir) walks)
                            )

                            (if (inbounds dpos row_len col_len)
                                (push (list dpos downdir) walks)
                            )
                        ))
                    ((eql obj #\/)
                        (let ((upos (mapcar '+ pos updir))
                              (dpos (mapcar '+ pos downdir))
                              (lpos (mapcar '+ pos leftdir))
                              (rpos (mapcar '+ pos rightdir)))
                            (cond 
                                ((equal dir updir)
                                    (if (inbounds rpos row_len col_len)
                                        (push (list rpos rightdir) walks)
                                    )
                                )
                                ((equal dir downdir)
                                    (if (inbounds lpos row_len col_len)
                                        (push (list lpos leftdir) walks)
                                    )
                                )
                                ((equal dir leftdir)
                                    (if (inbounds dpos row_len col_len)
                                        (push (list dpos downdir) walks)
                                    )
                                )
                                ((equal dir rightdir)
                                    (if (inbounds upos row_len col_len)
                                        (push (list upos updir) walks)
                                    )
                                )
                                (t (assert nil))
                            )
                        ))
                    ((eql obj #\\)
                        (let ((upos (mapcar '+ pos updir))
                              (dpos (mapcar '+ pos downdir))
                              (lpos (mapcar '+ pos leftdir))
                              (rpos (mapcar '+ pos rightdir)))
                            (cond 
                                ((equal dir updir)
                                    (if (inbounds lpos row_len col_len)
                                        (push (list lpos leftdir) walks)
                                    )
                                )
                                ((equal dir downdir)
                                    (if (inbounds rpos row_len col_len)
                                        (push (list rpos rightdir) walks)
                                    )
                                )
                                ((equal dir leftdir)
                                    (if (inbounds upos row_len col_len)
                                        (push (list upos updir) walks)
                                    )
                                )
                                ((equal dir rightdir)
                                    (if (inbounds dpos row_len col_len)
                                        (push (list dpos downdir) walks)
                                    )
                                )
                                (t (assert nil))
                            )
                        ))
                )
            :end
        )
        visited
    )
)

(setf lparallel:*kernel* (lparallel:make-kernel 8))
(format t "input.txt: ~a~%"
    (let* ((input (parse "input.txt"))
           (row_len (first  input))
           (row_max (- row_len 1))
           (col_len (second input))
           (col_max (- col_len 1))
           (objects (third  input))
           (search_args
                (append
                    (loop for col_idx from 0 below col_len collect
                        (list (list 0 col_idx) downdir))
                    (loop for col_idx from 0 below col_len collect
                        (list (list row_max col_idx) updir))
                    (loop for row_idx from 0 below row_len collect
                        (list (list row_idx 0) rightdir))
                    (loop for row_idx from 0 below row_len collect
                        (list (list row_idx col_max) leftdir))
                )))
        (apply 'max 
            (lparallel:pmap
                'list
                (lambda (args)
                    (hash-table-count
                        (traverse-beam (first args) (second args) row_len col_len objects))
                )
                search_args)
        )
    )
)
