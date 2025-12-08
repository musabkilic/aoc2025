(require :asdf)

(defvar *input* (uiop:read-file-lines "input"))

(defun ans1 (*input*)
  (let ((n (length *input*))
        (m (length (first *input*)))
        (beam (list (position #\S (first *input*))))
        (split 0))
    (loop for i from 1 below n do
      (let ((new-beam '()))
        (loop for b in beam
          if (char= (char (nth i *input*) b) #\^)
            do (pushnew (1- b) new-beam)
            and do (pushnew (1+ b) new-beam)
            and do (incf split)
          else
            do (pushnew b new-beam)
        )
        (setf beam new-beam)
      )
    )
    split
  )
)

(defun ans2 (*input*)
  (let ((n (length *input*))
        (m (length (first *input*)))
        (beam (make-hash-table :test 'equal)))
    (incf (gethash (position #\S (first *input*)) beam 0))
    (loop for i from 1 below n do
      (let ((new-beam (make-hash-table :test 'equal)))
        (loop for b being the hash-keys of beam
          using (hash-value n)
          if (char= (char (nth i *input*) b) #\^)
            do (incf (gethash (1- b) new-beam 0) n)
            and do (incf (gethash (1+ b) new-beam 0) n)
          else
            do (incf (gethash b new-beam 0) n)
        )
        (setf beam new-beam)
      )
    )
    (loop for n being the hash-values of beam
          sum n)
  )
)

(print (ans1 *input*))
(print (ans2 *input*))
(terpri)