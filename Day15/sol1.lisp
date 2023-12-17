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

(defun calculate-hash-sum(filename)
    (with-open-file (file filename :direction :input)
        (lparallel:preduce '+
            (lparallel:pmapcar
                (lambda (s)
                    (reduce (lambda (total code) (rem (* (+ code total) 17) 256))
                        (loop for c across s collect (char-code c))
                        :initial-value 0))
                (uiop:split-string (read-line file nil) :separator ",")
            )
        )
    )
)

(setf lparallel:*kernel* (lparallel:make-kernel 8))
(format t "sample.txt: ~a~%" (calculate-hash-sum "sample.txt"))
(format t "input.txt: ~a~%" (calculate-hash-sum "input.txt"))
