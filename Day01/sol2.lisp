(defvar numbers '(0 1 2 3 4 5 6 7 8 9))
(defvar digits '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
(defvar ordinals '("zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))
(defvar left_index)
(defvar left_indexes)
(defvar left_number)
(defvar right_index)
(defvar right_indexes)
(defvar right_number)
(defvar aux)

(defvar total 0)

(with-open-file (file "input.txt" :direction :input)
    (loop for word = (read-line file nil) while word do
        (setq left_indexes nil)
        (setq right_indexes nil)
        (loop for n in numbers
              for c in digits
              for s in ordinals
            do 
             (let ((idx1 (search c word))
                   (idx2 (search s word))
                   (idx3 (search c word :from-end t))
                   (idx4 (search s word :from-end t)))
                (cond
                    ((and idx1 idx2)
                        (push (min idx1 idx2) left_indexes))
                    (idx1
                        (push idx1 left_indexes))
                    (idx2
                        (push idx2 left_indexes))
                    (t
                        (push nil left_indexes)))
                (cond
                    ((and idx3 idx4)
                        (push (max idx3 idx4) right_indexes))
                    (idx3
                        (push idx3 right_indexes))
                    (idx4
                        (push idx4 right_indexes))
                    (t
                        (push nil right_indexes)))
             )
        )
        (setq aux 9)
        (setq left_index nil)
        (setq left_number nil)
        (loop for i in left_indexes do
            (cond
                ((not i)
                    ; skip nil
                )
                ((not left_index)
                    ; init value
                    (setq left_index i)
                    (setq left_number aux)
                )
                ((< i left_index)
                    (setq left_index i)
                    (setq left_number aux)
                )
            )
            (setq aux (- aux 1))
        )

        (setq aux 9)
        (setq right_index nil)
        (setq right_number nil)
        (loop for j in right_indexes do
            (cond
                ((not j)
                    ; skip nil
                )
                ((not right_index)
                    ; init value
                    (setq right_index  j)
                    (setq right_number aux)
                )
                ((> j right_index)
                    (setq right_index j)
                    (setq right_number aux)
                )
            )
            (setq aux (- aux 1))
        )
        (setq total (+ (* left_number 10) right_number total))
        ;(format T "~a ~a ~a~%" word left_number right_number)
    )
    (format T "~a~%" total)
)