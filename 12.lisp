(require :asdf)

(defvar *input* (uiop:read-file-lines "input"))

(defun ans1 (*input*)
  (let* ((shapes (loop
          for i below 6
          collect (make-array '(3 3) :element-type 'bit :initial-contents (map 'vector (lambda (l) (map 'vector (lambda (x) (if (string= x "#") 1 0)) l)) (subseq *input* (+ 1 (* 5 i)) (+ 4 (* 5 i)))))
        ))
        (regions (mapcar (lambda (l) (list 
                            (mapcar #'parse-integer (uiop:split-string (subseq l 0 (position #\: l)) :separator "x"))
                            (map 'vector #'parse-integer (uiop:split-string (subseq l (+ 2 (position #\: l))) :separator " "))
                          ))
                  (subseq *input* 30)))
        (shapesfr (loop
            for shape in shapes
            collect (remove-duplicates (loop
              for fshape in (loop
                for (tj ti) in '((0 0) (0 1) (1 0) (1 1))
                for (sj dj) = (list (* 2 tj) (1+ (* tj -2)))
                for (si di) = (list (* 2 ti) (1+ (* ti -2)))
                collect (make-array '(3 3) :element-type 'bit :initial-contents (loop 
                  with j = sj
                  for _ below 3
                  for i = si
                  collect (loop
                    for __ below 3
                    collect (aref shape j i)
                    do (incf i di)
                  )
                  do (incf j dj)
                ))
              )
              collect fshape
              collect (make-array '(3 3) :element-type 'bit :initial-contents (loop
                for i downfrom 2 to 0
                collect (loop
                  for j below 3
                  collect (aref fshape i j)
                )
              ))
              collect (make-array '(3 3) :element-type 'bit :initial-contents (loop
                for j downfrom 2 to 0
                collect (loop
                  for i below 3
                  collect (aref fshape i j)
                )
              ))
            ) :test (lambda (a b)
              (loop
                for j below 9
                always (= (row-major-aref a j) (row-major-aref b j))
              )
            ))
          into lv
          finally (return (coerce lv 'vector))
        ))
        (shapesize (coerce (loop for shape in shapes collect (loop for i below 9 sum (row-major-aref shape i))) 'vector))
    )
    (loop
      for ((m n) pieces) in regions
      when (let ((region (make-array `(,n ,m) :initial-element 0)))
        (labels ((check (reg i j pcs tpcs npcs)
          (when (zerop tpcs)
            (return-from check t)
          )
          (loop
            for j from j below (- m 2)
            when (< (* (- m j) n) tpcs)
              do (return-from check)
            when (>= (* (floor n 3) (floor (- m j) 3)) npcs)
              do (return-from check t)
            do (loop
              for i from i below (- n 2)
              do (loop
                for ip below 6
                unless (zerop (aref pcs ip))
                do (loop
                  for pfr in (aref shapesfr ip)
                  when (loop
                    for dj below 3
                    always (loop
                      for di below 3
                      never (and (= 1 (aref pfr di dj)) (= 1 (aref reg (+ i di) (+ j dj))))
                    )
                  )
                    do (loop
                      for dj below 3
                      do (loop
                        for di below 3
                        when (= (aref pfr di dj) 1)
                        do (incf (aref reg (+ i di) (+ j dj)))
                      )
                    )
                    and do (decf (aref pcs ip))
                    and do (when (check reg i (1+ j) pcs (- tpcs (aref shapesize ip)) (1- npcs)) (return-from check t))
                    and do (incf (aref pcs ip))
                    and do (loop
                      for dj below 3
                      do (loop
                        for di below 3
                        when  (= (aref pfr di dj) 1)
                        do (decf (aref reg (+ i di) (+ j dj)))
                      )
                    )
                ) 
              )
            ) (setf i 0)
          )
          ))
          (check region 0 0 pieces (loop for i below 6 sum (* (aref pieces i) (aref shapesize i))) (reduce #'+ pieces))
        )
      )
      sum 1
    )
  )
)

(defun ans2 (*input*)
  "Happy Holidays"
)

(print (time (ans1 *input*)))
(print (time (ans2 *input*)))
(terpri)