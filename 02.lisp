(require :asdf)

(defvar *input* (uiop:read-file-string "input"))

(defun ans1 (*input*)
    (apply '+
        (mapcar 
            (lambda (r)
                (apply '+
                    (multiple-value-bind (a b) 
                        (values-list (mapcar #'parse-integer (uiop:split-string r :separator "-")))
                        (loop while (<= a b)
                        collect
                            (apply '+
                                (let* (
                                    (l (+ (- 1 (mod (floor (log a 10)) 2)) (floor (log a 10))))
                                    (mask (+ 1 (expt 10 (/ (+ l 1) 2 ))))
                                    (first-el (* mask (expt 10 (/ (- l 1) 2))))
                                    (fi (max (+ a (if (= 0 (mod a mask)) 0 (- mask (mod a mask)))) first-el))
                                    (la (min b (- (expt 10 (+ l 1)) 1))))
                                
                                    (setf a (expt 10 (+ l 2)))
                                    (loop while (<= fi la)
                                    collect fi
                                    do (incf fi mask)
                                    )
                                )
                            )
                        )
                    )
                )
            )
        (uiop:split-string *input* :separator ",")
        )
    )
)

(defun ans2 (*input*)
    (apply '+
        (mapcar 
            (lambda (r)
                (apply '+
                    (multiple-value-bind (x y) 
                        (values-list (mapcar #'parse-integer (uiop:split-string r :separator "-")))
                        (loop while (<= x y)
                        collect 
                            (let* ((i 1) (xs (write-to-string x)) (lxs (length xs)) (flag t))
                                (loop while (and (<= i (/ lxs 2)) flag)
                                do
                                    (when (= 0 (mod lxs i))
                                        (let ((fs (subseq xs 0 i)) (j i))
                                            (loop while (and (< j lxs) (string= fs (subseq xs j (+ j i))))
                                            do (incf j i)
                                            )
                                            (if (>= j lxs)
                                                (setf flag nil)
                                            )
                                        )
                                    )
                                    (incf i)
                                )
                                (incf x)
                                (if flag 0 (- x 1))
                            )
                        )
                    )
                ))
            (uiop:split-string *input* :separator ",")
        )
    )
)

(print (ans1 *input*))
(print (ans2 *input*))
(terpri)