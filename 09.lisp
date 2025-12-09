(require :asdf)

(defvar *input* (uiop:read-file-lines "input"))

(defun ans1 (*input*)
  (let* ((tiles (map 'vector (lambda (x) (mapcar #'parse-integer (uiop:split-string x :separator ","))  ) *input*))
         (n (length tiles)))
    
    (loop for i below n
        maximize (loop for j from (1+ i) below n
                  maximize (* (1+ (abs (- (first (aref tiles i)) (first (aref tiles j)))))
                              (1+ (abs (- (second (aref tiles i)) (second (aref tiles j))))) ))
    )
  )
)

(defun ans2 (*input*)
  (let* ((tiles (map 'vector (lambda (x) (mapcar #'parse-integer (uiop:split-string x :separator ","))  ) *input*))
         (n (length tiles))
         (vert (sort (loop for i below n
                        for j = (mod (1+ i) n)
                        for (x1 y1) = (aref tiles i)
                        for (x2 y2) = (aref tiles j)
                        when (= x1 x2)
                        collect (list x1 (min y1 y2) (max y1 y2)))
                    #'< :key #'first))
         (horz (sort (loop for i below n
                        for j = (mod (1+ i) n)
                        for (x1 y1) = (aref tiles i)
                        for (x2 y2) = (aref tiles j)
                        when (= y1 y2)
                        collect (list y1 (min x1 x2) (max x1 x2)))
                    #'< :key #'first))
        )
    (loop 
        for i below n
        for (a b) = (aref tiles i)
        maximize (loop
            for z from (1+ i) below n
            for (c d) = (aref tiles z)
            for (x1 x2 y1 y2) = (list (min a c) (max a c) (min b d) (max b d))
            when (loop
                for (vx vy1 vy2) in vert
                when (and (< x1 vx x2)
                          (> (- (min vy2 y2) (max vy1 y1)) 0))
                    return nil
                finally (return t))
            when (loop
                for (vy vx1 vx2) in horz
                when (and (< y1 vy y2)
                          (> (- (min vx2 x2) (max vx1 x1)) 0))
                    return nil
                finally (return t))
            maximize (* (1+ (- x2 x1)) (1+ (- y2 y1)))
        )
    )
  )
)

(print (time (ans1 *input*)))
(print (time (ans2 *input*)))
(terpri)