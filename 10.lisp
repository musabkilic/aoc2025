(require :asdf)

(defvar *input* (uiop:read-file-lines "input"))

(defun ans1 (*input*)
  (let ((machines (mapcar (lambda (l) 
      (let* 
        ((d1 (position #\[ l))
        (d2 (position #\] l))
        (d3 (position #\{ l))
        (d4 (position #\} l)))
        
        (list 
          (parse-integer (reverse (substitute #\0 #\. (substitute #\1 #\# (subseq l (1+ d1) d2)))) :radix 2)
          (map 'vector (lambda (r) (apply #'logxor (mapcar (lambda (x) (ash 1 (parse-integer x))) (uiop:split-string (subseq r 1 (1- (length r))) :separator ",")))) (uiop:split-string (subseq l (+ d2 2) (1- d3)) :separator " "))
          (mapcar #'parse-integer (uiop:split-string (subseq l (1+ d3) d4) :separator ","))
        )
      )     
    ) *input*)))
    
    (loop for (target buttons joltages) in machines
    sum
      (let 
        ((ss '((0 0)))
         (nss '())
         (n (length buttons))
        )
        (loop 
          for c from 0
          when (loop
            for (i s) in ss
            when (= s target)
              return t
            do 
            (loop
              for j from i below n
              do (push (list (1+ j) (logxor s (aref buttons j))) nss)
            )
          )
            return c
          do (setf ss nss nss '())
        )
      )
    )
  )
)

(defun ans2 (*input*)
  (let ((machines (mapcar (lambda (l) 
      (let* 
        ((d1 (position #\[ l))
        (d2 (position #\] l))
        (d3 (position #\{ l))
        (d4 (position #\} l)))
        
        (list 
          (parse-integer (reverse (substitute #\0 #\. (substitute #\1 #\# (subseq l (1+ d1) d2)))) :radix 2)
          (mapcar (lambda (r) (mapcar #'parse-integer (uiop:split-string (subseq r 1 (1- (length r))) :separator ","))) (uiop:split-string (subseq l (+ d2 2) (1- d3)) :separator " "))
          (map 'vector #'parse-integer (uiop:split-string (subseq l (1+ d3) d4) :separator ","))
        )
      )     
    ) *input*)))
    (loop
    for (target ubuttons ujoltages) in machines
    sum
      (let* 
        ((n (length ubuttons))
         (l (length ujoltages))
         (joltagesnbuttons (sort (loop
              for j below l
              for N = (aref ujoltages j)
              for degree = (loop for bb in ubuttons count (find j bb))
              collect (list j degree N))
              (lambda (a b) 
                (if (= (second a) (second b))
                    (> (third a) (third b))
                    (< (second a) (second b))))))
         (sind (map 'vector #'first joltagesnbuttons))
         (joltages (map 'vector (lambda (x) (aref ujoltages x)) sind))
         (buttons (sort (loop
            for bb in ubuttons
            collect (sort (loop
              for b in bb
              collect (position b sind)
              into lvv
              finally (return (coerce lvv 'vector))
            ) #'<)
            into lv
            finally (return (coerce lv 'vector))
          )
          (lambda (a b) (loop
            for x across a
            for y across b
            do (cond ((< x y) (return t))
                      ((> x y) (return nil)))
            finally (return (< (length a) (length b)))))
         ))
         (latest-button (loop
            for j below l
            collect (loop
              for k downfrom (1- n) to 0
              for bb = (aref buttons k)
              when (find j bb)
              return k
            )
            into lv
            finally (return (coerce lv 'vector))
         ))
         (expires-at (let ((arr (make-array n :initial-element nil)))
                     (loop for k from 0 below l
                            for c = (aref latest-button k)
                            do (push k (aref arr c)))
                      arr))
         (best-sol (- (reduce #'+ joltages) (loop for i below l maximize (* (aref joltages i) (1- (loop for bb across buttons when (find i bb) minimize (length bb))))))) 
        )
        (labels ((solve (i c m s)
          (when (>= i best-sol)
            (return-from solve))
          (when (zerop m)
            (setf best-sol i)
            (return-from solve)
          )
          (when (= c n)
            (return-from solve)
          )
          
          (let* ((bb (aref buttons c))
                (max-v (loop for b across bb minimize (aref s b)))
                (lb (length bb)))

            (let ((forced-v nil))
              (dolist (k (aref expires-at c))
                (let ((req (aref s k)))
                  (when (> req 0)
                    (if forced-v
                        (unless (= forced-v req) (return-from solve))
                        (setf forced-v req)))))
              (when forced-v
                (if (> forced-v max-v) 
                    (return-from solve)
                    (progn
                      (loop for b across bb do (decf (aref s b) forced-v))
                      (solve (+ i forced-v) (1+ c) (- m (* forced-v lb)) s)
                      (loop for b across bb do (incf (aref s b) forced-v))
                      (return-from solve)
                    )
                )
              )
            )
            (loop for b across bb do (decf (aref s b) max-v))
            (loop
              for v downfrom max-v to 0
              do 
                (solve (+ i v) (1+ c) (- m (* v lb)) s)
                (loop for b across bb do (incf (aref s b)))
            )
            (loop for b across bb do (decf (aref s b)))
          )
        ))
        (solve 0 0 (reduce #'+ joltages) joltages)
        )
        best-sol
      )
    )
  )
)

(defun ans2-ilp (*input*)
  (let ((machines (mapcar (lambda (l) 
      (let* 
        ((d1 (position #\[ l))
        (d2 (position #\] l))
        (d3 (position #\{ l))
        (d4 (position #\} l)))
        
        (list 
          (parse-integer (reverse (substitute #\0 #\. (substitute #\1 #\# (subseq l (1+ d1) d2)))) :radix 2)
          (mapcar (lambda (r) (mapcar #'parse-integer (uiop:split-string (subseq r 1 (1- (length r))) :separator ","))) (uiop:split-string (subseq l (+ d2 2) (1- d3)) :separator " "))
          (map 'vector #'parse-integer (uiop:split-string (subseq l (1+ d3) d4) :separator ","))
        )
      )     
    ) *input*)))

    (loop for (target buttons joltages) in machines
          for index from 0
          sum
          (let* ((n (length buttons))
                 (l (length joltages))
                 (lp-input (make-string-output-stream)))

            (format lp-input "min obj: ")
            (loop for i below n do (format lp-input "+ x~D " i))
            (format lp-input "~%st~%")

            (loop for j below l
                  for req = (aref joltages j)
                  do
                  (loop for i below n
                          when (find j (nth i buttons))
                          do (format lp-input "+ x~D " i))
                  (format lp-input " = ~D~%" req))

            (format lp-input "bounds~%")
            (loop for i below n do (format lp-input " x~D >= 0~%" i))
            
            (format lp-input "integers~%")
            (loop for i below n do (format lp-input " x~D~%" i))
            (format lp-input "end")

            (let* ((lp-string (get-output-stream-string lp-input))
                   (output (with-output-to-string (s)
                      (uiop:run-program 
                        '("glpsol" "--lp" "/dev/stdin" "-o" "/dev/stdout")
                        :input (make-string-input-stream lp-string)
                        :output s)))
                   (sobj (search "Objective:  obj = " output))
                   (eobj (search "(MINimum)" output :start2 sobj))
                  )
              
              (parse-integer (subseq output (+ 17 sobj) eobj))
            )
          )
    )
  )
)

(print (time (ans1 *input*)))
(print (time (ans2 *input*)))
(print (time (ans2-ilp *input*)))
(terpri)