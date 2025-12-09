(require :asdf)

(defvar *lines* (uiop:read-file-lines "input"))

(defun ans1 (*lines*)
    (let ((*dial* 50))
        (apply '+ (loop for *l* in *lines*
            collect
                (if 
                    (= 0 (setf *dial* (mod (+ *dial* 
                        (* 
                        (if (char= (char *l* 0) #\L) -1 1)
                        (parse-integer (subseq *l* 1)))
                    ) 100)))
                    1
                    0
                )
        ))
    )
)

(defun ans2 (*lines*)
    (let ((*ndial*) (*dial* 50) (*ans* 0))
        (loop for *l* in *lines*
            do
                (setf *ndial* (+ *dial* 
                    (* 
                    (if (char= (char *l* 0) #\L) -1 1)
                    (parse-integer (subseq *l* 1)))
                ))
                (if (and (= 0 *dial*) (< *ndial* 0)) (decf *ans*))
                
                (loop while (< *ndial* 0)
                do (setf *ndial* (+ *ndial* 100)) (incf *ans*))

                (if (= 0 *ndial*) (incf *ans*))

                (loop while (>= *ndial* 100)
                do (setf *ndial* (- *ndial* 100)) (incf *ans*))
                (setq *dial* *ndial*)
        )
        *ans*
    )
)

(print (time (ans1 *input*)))
(print (time (ans2 *input*)))
(terpri)