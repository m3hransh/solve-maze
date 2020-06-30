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

;; helper function for finding adjacency grid that we can go from index
;; with respect to the path that we've visted
(defun get-neighbors(index path)
    (let ((neighbors nil))
    ;; check up
    (let (( up (list (- (nth 0 index) 1) (nth 1 index)) )) 
        (if (and (>= (nth 0 up) 0)                       ; if the index is in map
            (equal (aref *maze* (nth 0 up) (nth 1 up)) 0); there is no obstacle
            (null (gethash up path))                     ; it's not visited before
        )
          (push (list up 'U) neighbors)
        )
    )
    ;; check right
    (let (( right  (list (nth 0 index) (+ (nth 1 index) 1)) ))
        (if (and (< (nth 1 right) M)
            (equal (aref *maze* (nth 0 right) (nth 1 right)) 0)
            (null (gethash right path))
        )
        (push (list right 'R) neighbors)
        )
    )
    ;; check below
    (let (( down  (list (+ (nth 0 index) 1) (nth 1 index)) ))
        (if (and (<  (nth 0 down)  N)
            (equal (aref *maze* (nth 0 down) (nth 1 down)) 0)
            (null (gethash down path))
        )
        (push (list down 'D) neighbors)
        )
    )
    ;; check left
    (let (( left (list (nth 0 index) (- (nth 1 index) 1)))) 
        (if (and (>= (nth 1 left) 0) 
            (equal (aref *maze* (nth 0 left) (nth 1 left)) 0)
            (null (gethash left path))
        )
          (push (list left 'L) neighbors)
        )
    )
    (return-from get-neighbors neighbors);return all possible neighbor accessible from index
    )
)

;; solve the maze by getting 2d-array as map
;; and start as (x y) that specifies the staring point
;; and end for end point
(defun solve-maze(maze start end)
    ;; path hash-table for stroing the path we take to reach to each grid
    ;; path[p1] = (p0 L) says that we reach to p1 from p0 by going to the left
    (setq path (make-hash-table :test 'equal) )
    ;; stack for visiting grid recursively
    (setq stack nil) 
    (push start stack)
    ;; start 
    (setf (gethash start path) (list -1 -1))
    (let ((p
    (loop
        ;; taking the first element to expand its accessible neighbours
        (setq el  (car stack))
        (setf stack (cdr stack))
        ;; the loop finish when the el is the end point
        ;; or the stack is empty and that means there is no path to goal
        (when (or (null el) (equal el end)) (return path))
        ;; geting el's neighbors
        (let ((ns (get-neighbors el path) ))
            (if (not (equal ns nil))
                (dolist (neighbor ns)
                (progn
                    ;; ading the neighbor to the stack
                    ;; and to the path
                    (push (car neighbor) stack)
                    (setf (gethash (car neighbor) path) (list el (cadr neighbor)))
                )
                )
                
            )
        
        )
    ) ))
    ;; if there is a path to end point
    ;; it we'll print the plan
    (if (gethash end path)
        (progn
            (setq  temp end)
            (setq plan nil)
            (loop (when (equal temp start) (return  plan))
                (push (cadr (gethash temp path)) plan)
                
                (setf temp (car (gethash temp path)))
            )
        )
        (format t "no path is found!~%")
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
;; solve the with starting point (0 0) and end point of (3 0)
(format t "Plan to find the goal:~% ~a" (solve-maze *maze* '(0 0) '(3 4)))
