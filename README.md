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
Then run following command on terminal:
```
$ clisp maze_solver.lisp
```

In this situation you will get the following output:
```
Map:
0 0 0 0 0 
1 1 0 1 0 
1 0 0 1 0 
0 0 1 1 0 

Plan to find the goal:
 (R R R R D D D)
```