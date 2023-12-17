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
        (lparallel:pmapcar
            (lambda (s)
                (let* ((subs (uiop:split-string s :separator "-=")))
                    (list
                        (first subs)
                        (reduce (lambda (total code) (rem (* (+ code total) 17) 256))
                            (loop for c across (first subs) collect (char-code c))
                            :initial-value 0)
                        (if (not (string= "" (second subs))) (parse-integer (second subs)) 0)
                    )
                )
            )
            (uiop:split-string (read-line file nil) :separator ",")
        )
    )
)

(defun compute(instructions)
    (let ((boxes (make-hash-table))
          (result nil))
        (loop for instr in instructions
              for label = (first  instr)
              for hash  = (second instr)
              for lens  = (third  instr)
              for box   = (gethash hash boxes)
              do
            (cond
                ; empty box
                ((and (eql nil box) (> lens 0))
                    (setf (gethash hash boxes) (list (list label lens))))

                ; remove label
                ((eql lens 0)
                    (setf (gethash hash boxes) (delete label box :key 'first :test 'string=)))

                ; update label lens
                ((find label box :key 'first :test 'string=)
                    (setf
                        (nth (position label (gethash hash boxes) :key 'first :test 'string=) (gethash hash boxes))
                        (list label lens)))

                ; insert new entry
                (t
                    (push (list label lens) (gethash hash boxes)))
            )
        )
        (setq result 
            (loop for k being each hash-key of boxes
              for box = (gethash k boxes)
              :if box :collect (list k (reverse (gethash k boxes)))))
        (reduce
            (lambda (total entry)
                (+ total (apply '+
                    (loop for pos from 1
                        for box = (+ (first entry) 1)
                        for slot in (second entry)
                        for lens = (second slot)
                        collect (* pos lens box))
                    )))
            result
            :initial-value 0)
    )
)

(setf lparallel:*kernel* (lparallel:make-kernel 8))
(format t "sample.txt: ~a~%" (compute (parse "sample.txt")))
(format t "input.txt: ~a~%" (compute (parse "input.txt")))
