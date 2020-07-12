# solve-maze
### rquirement
Install common lisp in your machine.
### executing
There is a [maze.text](maze.txt) file, there you specify your maze. like the following:
```
0 0 0 0 0 
1 1 0 1 0 
1 0 0 1 0
0 0 1 1 0
```
In the [maze_solver.lisp](maze_solver.lisp) source file determine your start and end points as arguemnt to function solve maze:
``` lisp
;; solve the with starting point (0 0) and end point of (3 0)
(format t "Plan to find the goal:~% ~a" (solve-maze  (cons 0 0) (cons 3 0) (list (cons (cons 0 0) 'S)) ))
```
Then run following command on terminal:
``` bash
$ clisp maze_solver.lisp
```

In this situation you will get the following output:
``` bash
Map:
0 0 0 0 0 
1 1 0 1 0 
1 0 0 1 0 
0 0 1 1 0 

start:(0 0) 
Goal:(3 0) 

Plan to find the goal:
 (R R D D L D L)
```