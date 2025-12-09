(require :asdf)

(defvar *input* (uiop:read-file-lines "input"))

(defun ans1 (*input*)
  (let* ((boxes (map 'vector (lambda (x) (map 'vector #'parse-integer (uiop:split-string x :separator ","))) *input*))
         (n (length boxes))
         (dist (sort (loop for i from 0 below n
                    append
                      (loop for j from (1+ i) below n
                            collect (list (+ (expt (- (aref (aref boxes i) 0) (aref (aref boxes j) 0)) 2)
                                             (expt (- (aref (aref boxes i) 1) (aref (aref boxes j) 1)) 2)
                                             (expt (- (aref (aref boxes i) 2) (aref (aref boxes j) 2)) 2))
                                          i j)))
                #'< :key #'first))
         (circuits (make-array n :initial-contents (loop for i below n collect i)))
         (circuit-sizes (make-array n :initial-element 1)))
    (loop
      for i from 0 below 1000
      for (d a b) = (nth i dist) 
      do
        (loop while (not (= (aref circuits a) a))
          do  (setf a (aref circuits a)))
        (loop while (not (= (aref circuits b) b))
          do  (setf b (aref circuits b)))
        (when (not (= a b))
          (setf (aref circuits (min a b)) (max a b)))
    )
    (loop
      for i below n
      when (/= (aref circuits i) i)
      do
        (incf (aref circuit-sizes (aref circuits i)) (aref circuit-sizes i))
    )
    (apply #'*
      (subseq (sort (loop
        for i below n
        when (= (aref circuits i) i)
        collect (aref circuit-sizes i))
      #'>) 0 3))
  )
)

(defun ans2 (*input*)
  (let* ((boxes (map 'vector (lambda (x) (map 'vector #'parse-integer (uiop:split-string x :separator ","))) *input*))
         (n (length boxes))
         (dist (sort (loop for i from 0 below n
                    append
                      (loop for j from (1+ i) below n
                            collect (list (+ (expt (- (aref (aref boxes i) 0) (aref (aref boxes j) 0)) 2)
                                             (expt (- (aref (aref boxes i) 1) (aref (aref boxes j) 1)) 2)
                                             (expt (- (aref (aref boxes i) 2) (aref (aref boxes j) 2)) 2))
                                          i j)))
                #'< :key #'first))
         (circuits (make-array n :initial-contents (loop for i below n collect i)))
         (nconn 0))
    (loop
      for i from 0 below (length dist)
      for (d k l) = (nth i dist)
      for (a b) = (list k l)
      do
        (loop while (not (= (aref circuits a) a))
          do  (setf a (aref circuits a)))
        (loop while (not (= (aref circuits b) b))
          do  (setf b (aref circuits b)))
        (when (not (= a b))
          (setf (aref circuits (min a b)) (max a b))
          (incf nconn))
      when (= nconn (- n 1))
        return (* (aref (aref boxes k) 0) (aref (aref boxes l) 0))
    )
  )
)

(print (time (ans1 *input*)))
(print (time (ans2 *input*)))
(terpri)