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
In the [maze_solver.lisp](maze_solver.lisp) source file determine your start and end points:
``` lisp
(setq *start* '(0 0))
(setq *end* '(3 0))
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