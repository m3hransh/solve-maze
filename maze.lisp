(setq *print-case* :capitalize)
;; defining fucntion for spliting line by space
(defun delimiterp (c) (or (char= c #\Space) (char= c #\,)))
(defun my-split (string &key (delimiterp #'delimiterp))
  (loop :for beg = (position-if-not delimiterp string)
    :then (position-if-not delimiterp string :start (1+ end))
    :for end = (and beg (position-if delimiterp string :start beg))
    :when beg :collect (subseq string beg end)
    :while end))
;; *maze* variable for storing our map az matrix
(setq *maze* nil)
;;define start and end points here

;; reading from the fiel maze.txt
(let ((in (open "maze.txt" :if-does-not-exist nil)))
   (when in
      (loop for line = (read-line in nil)
      
      while line do (push  (mapcar #'parse-integer (my-split line)) *maze*))
      (close in)
   )
)
;; function for converting 2d list to 2d-array
(defun list-to-2d-array (list)
  (make-array (list (length list)
                    (length (first list)))
              :initial-contents list))
;; shape size of the maze
(defconstant N (length *maze*))
(defconstant M (length (first *maze*)))
;; reverse tge *maze* and convert to the array
(setf *maze* (list-to-2d-array (reverse *maze*)))


(defun check-path(x y)
    (cond ((atom y) nil)
    ((equal x (caar y)) x)
    (t (check-path x (cdr y)))
    )
)


;; helper function for finding adjacency grid that we can go from index
;; with respect to the path that we've visted
(defun get-neighbors(index path)
    (let ((neighbors nil))
    ;; check up
    (let (( up (cons (- (car index) 1) (cdr index)) )) 
        (if (and (>= (car up) 0)                       ; if the index is in map
            (equal (aref *maze* (car up) (cdr up)) 0); there is no obstacle
            (null (check-path up path))                     ; it's not visited before
        )
          (push (cons up 'U) neighbors)
        )
    )
    ;; check right
    (let (( right  (cons (car index) (+ (cdr index) 1)) ))
        (if (and (< (cdr right) M)
            (equal (aref *maze* (car right) (cdr right)) 0)
            (null (check-path right path))
        )
        (push (cons right 'R) neighbors)
        )
    )
    ;; check below
    (let (( down  (cons (+ (car index) 1) (cdr index)) ))
        (if (and (<  (car down)  N)
            (equal (aref *maze* (car down) (cdr down)) 0)
            (null (check-path down path))
        )
        (push (cons down 'D) neighbors)
        )
    )
    ;; check left
    (let (( left (cons (car index) (- (cdr index) 1)))) 
        (if (and (>= (cdr left) 0) 
            (equal (aref *maze* (car left) (cdr left)) 0)
            (null (check-path left path))
        )
          (push (cons left 'L) neighbors)
        )
    )
    (return-from get-neighbors neighbors);return all possible neighbor accessible from index
    )
)


(defun search-neighbors(neighbors start end path solve-maze)
        (cond ((null (car neighbors)) nil) ;if there is no neighbor it returns nil
            (t ((lambda (x) ; to check if the first neighbor give the answer if not search other neighbors
                    (cond ((null x) (search-neighbors (cdr neighbors) start end path solve-maze))
                        (t x) )
                ;; calling the solve-maze with start point of the neighbor 
                )(solve-maze (caar neighbors) end 
                    (append path (list (car neighbors)))
                    ; and new path that consist of previous
                 )  ; start and it's direction to the new start 
                )
                )  
        )
)

(defun plan-for-path(x)
    (cond ((null x) nil)
        ;; creating a list of directions of the path elements
        (t (cons (cdar x) (plan-for-path (cdr x)) )
            
        )
)
)

(defun solve-maze(start end path)
    (cond ((equal start end) (plan-for-path path))
            (t (search-neighbors (get-neighbors start path) start end path #'solve-maze)
            )
    )
)
;; printing the map
(format t "Map:~%")        
(dotimes (x 4)
    (dotimes(y 5)
        (format t "~a " (aref *maze* x y))
    )
    (format t "~%")
)
(terpri)
(format t "start:~a ~%" (cons 0 0)) 
(format t "Goal:~a ~%" (cons 3 0)) 
(terpri)
;; solve the with starting point (0 0) and end point of (3 0)
(format t "Plan to find the goal:~% ~a" (solve-maze  (cons 0 0) (cons 3 0) (list (cons (cons 0 0) 'S)) ))