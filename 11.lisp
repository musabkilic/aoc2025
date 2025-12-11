(require :asdf)

(defvar *input* (uiop:read-file-lines "input"))

(defun ans1 (*input*)
  (let ((wires 
         (let ((ht (make-hash-table :test 'equal))) 
          (loop
           for l in *input*
           for inp = (subseq l 0 3)
           for out = (uiop:split-string (subseq l 5) :separator " ")
           do (setf (gethash inp ht) out)
          )
          ht
         ))
        (path (make-hash-table))
        (npath (make-hash-table))
        (ans 0)
       )
    (setf (gethash "you" path) 1)
    (loop
      while (> (hash-table-count path) 0)
      do (loop
        for node being the hash-keys of path
        using (hash-value count)
        for out = (gethash node wires)
        do (loop
          for onode in out
          if (string= onode "out")
            do (incf ans count)
            else do (incf (gethash onode npath 0) count)
        )
      )
      (setf path npath npath (make-hash-table))
    )
    ans
  )
)

(defun ans2 (*input*)
  (let ((wires 
         (let ((ht (make-hash-table :test 'equal))) 
          (loop
           for l in *input*
           for inp = (subseq l 0 3)
           for out = (uiop:split-string (subseq l 5) :separator " ")
           do (setf (gethash inp ht) out)
          )
          ht
         ))
        (path (make-hash-table))
        (npath (make-hash-table))
        (ans 0)
       )
    (setf (gethash "svr" path) '(1 0 0 0))
    (loop
      while (> (hash-table-count path) 0)
      do (loop
        for node being the hash-keys of path
        using (hash-value counts)
        for out = (gethash node wires)
        for (c00 c0d cf0 cfd) = counts
        do (loop
          for onode in out
          for entry = (gethash onode npath)
          do
          (unless entry
            (setf entry (list 0 0 0 0))
            (setf (gethash onode npath) entry)
          )
          (cond
            ((string= onode "out")
             (incf ans cfd)
            )
            ((string= onode "dac")
             (incf (second entry) (+ c00 c0d))
             (incf (fourth entry) (+ cf0 cfd))
            )
            ((string= onode "fft")
             (incf (third entry) (+ c00 cf0))
             (incf (fourth entry) (+ c0d cfd))
            )
            (t
              (incf (first entry) c00)
              (incf (second entry) c0d)
              (incf (third entry) cf0)
              (incf (fourth entry) cfd)
            )
          )
        )
      )
      (setf path npath npath (make-hash-table))
    )
    ans
  )
)

(print (time (ans1 *input*)))
(print (time (ans2 *input*)))
(terpri)