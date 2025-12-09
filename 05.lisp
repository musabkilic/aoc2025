(require :asdf)

(defvar *input* (uiop:read-file-lines "input"))

(defun ans1 (*input*)
  (let* ((c (position "" *input* :test #'string=))
         (ranges (mapcar (lambda (x) (mapcar #'parse-integer (uiop:split-string x :separator "-"))) (subseq *input* 0 c)))
         (ingredients (mapcar #'parse-integer (subseq *input* (1+ c))))
        )
    (loop for i in ingredients
          sum (loop for (l r) in ranges
                    when (and (>= i l) (<= i r))
                      return 1
                    finally (return 0)
          )
    )
  )
)

(defun ans2 (*input*)
  (let ((ranges (sort (mapcar (lambda (x) (mapcar #'parse-integer (uiop:split-string x :separator "-"))) 
                              (subseq *input* 0 (position "" *input* :test #'string=)))
                      #'< :key #'first)))
    (loop for (l r) in ranges
          with curr = 0
          sum (1+ (max -1 (- r (max curr l))))
          when (>= r curr)
            do (setf curr (1+ r))
    )
  )
)

(print (time (ans1 *input*)))
(print (time (ans2 *input*)))
(terpri)