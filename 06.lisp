(require :asdf)

(defvar *input* (uiop:read-file-lines "input"))

(defun ans1 (*input*)
  (let* ((args (mapcar (lambda (l) (mapcar
            (lambda (x) (if (or (string= x "*") (string= x "+")) x (parse-integer x)))
            (remove-if (lambda (s) (string= s "")) (uiop:split-string l :separator " ")))) *input*)
        )
        (n (1- (length args)))
        (m (length (first args))))
    (loop for i from 0 below m
          sum
            (apply (if (string= (nth i (car (last args))) "*") #'* #'+)
              (loop for j from 0 below n
                    collect (nth i (nth j args)))
            )
    )
  ) 
)

(defun ans2 (*input*)
  (let* ((args *input*)
        (ops (car (last args)))
        (n (1- (length args)))
        (m (apply #'max (mapcar #'length args)))
        (k (length ops)))
    (loop for i from 0 below k
          when (string/= (char ops i) " ")
            sum
              (apply (if (string= (char ops i) "*") #'* #'+)
                (loop for i from i below m
                  for num = (coerce (loop for j from 0 below n
                                  for c = (if (< i (length (nth j args))) (char (nth j args) i) #\Space)
                                  when (char/= c #\Space)
                                  collect c) 'string)
                  until (string= num "")
                  collect (parse-integer num)
              ))
    )
  ) 
)

(print (time (ans1 *input*)))
(print (time (ans2 *input*)))
(terpri)