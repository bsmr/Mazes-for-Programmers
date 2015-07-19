# Mazes for Programmers - Erlang/OTP version

This is my [Erlang/OTP][] version of [Mazes for Programmers][].

[Erlang/OTP]: http://erlang.org/
[Mazes for Programmers]: https://pragprog.com/book/jbmaze/mazes-for-programmers

## Abstract

This project is based on the ideas given in the book
[Mazes for Programmers][], but it does not use lists or arrays to store
the grid and cells, instead it uses process to represent the data.

The application consists of a main supervisor. The main supervisor
spawns a Maze server and sub-supervisors for Grids and Cells.

[Mazes for Programmers]: https://pragprog.com/book/jbmaze/mazes-for-programmers

## Notes

A brief look at the `Makefile` should help. If `erlc` (and `erl`) is
in the `PATH` You should be able to issue `make run`.

```shell
bash$ make run
sed -e 's/%%VSN%%/0.0.1/g' <src/maze.app.in >ebin/maze.app
erlc -W -o ebin -I include -v src/maze_example.erl
erlc -W -o ebin -I include -v src/maze_util.erl
erlc -W -o ebin -I include -v src/maze_binary_tree.erl
erlc -W -o ebin -I include -v src/maze.erl
erlc -W -o ebin -I include -v src/maze_app.erl
erlc -W -o ebin -I include -v src/maze_sup.erl
erlc -W -o ebin -I include -v src/maze_srv.erl
erlc -W -o ebin -I include -v src/maze_grid.erl
erlc -W -o ebin -I include -v src/maze_grid_sup.erl
erlc -W -o ebin -I include -v src/maze_grid_srv.erl
erlc -W -o ebin -I include -v src/maze_cell.erl
erlc -W -o ebin -I include -v src/maze_cell_sup.erl
erlc -W -o ebin -I include -v src/maze_cell_srv.erl
erl -pa ebin -s maze_example start
Erlang/OTP 18 [erts-7.0.2] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V7.0.2  (abort with ^G)
1> 
=== Maze has 4 row(s) and 4 column(s) ===
+---++---++---++---+
|                  |
|                  |
|                  |
+   ++   ++---++   +
+   ++   ++---++   +
|   ||   ||        |
|   ||   ||        |
|   ||   ||        |
+   ++   ++   ++   +
+   ++   ++   ++   +
|   ||   ||   ||   |
|   ||   ||   ||   |
|   ||   ||   ||   |
+   ++   ++---++   +
+   ++   ++---++   +
|   ||   ||        |
|   ||   ||        |
|   ||   ||        |
+---++---++---++---+

bash$ 
```

Another example with `make run-10-20`:

```shell
bash$ make run-10-20
erl -pa ebin -s maze_example start -gridranks 10 20
Erlang/OTP 18 [erts-7.0.2] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V7.0.2  (abort with ^G)
1> 
=== Maze has 10 row(s) and 20 column(s) ===
+---++---++---++---++---++---++---++---++---++---++---++---++---++---++---++---++---++---++---++---+
|                                                                                                  |
|                                                                                                  |
|                                                                                                  |
+   ++   ++---++   ++   ++---++---++---++---++---++---++---++---++---++   ++   ++---++---++   ++   +
+   ++   ++---++   ++   ++---++---++---++---++---++---++---++---++---++   ++   ++---++---++   ++   +
|   ||   ||        ||   ||                                                ||   ||             ||   |
|   ||   ||        ||   ||                                                ||   ||             ||   |
|   ||   ||        ||   ||                                                ||   ||             ||   |
+---++   ++   ++   ++   ++   ++---++   ++   ++---++   ++---++---++   ++---++   ++---++   ++   ++   +
+---++   ++   ++   ++   ++   ++---++   ++   ++---++   ++---++---++   ++---++   ++---++   ++   ++   +
|        ||   ||   ||   ||   ||        ||   ||        ||             ||        ||        ||   ||   |
|        ||   ||   ||   ||   ||        ||   ||        ||             ||        ||        ||   ||   |
|        ||   ||   ||   ||   ||        ||   ||        ||             ||        ||        ||   ||   |
+   ++---++---++---++   ++   ++---++---++   ++   ++   ++   ++   ++---++   ++---++   ++   ++---++   +
+   ++---++---++---++   ++   ++---++---++   ++   ++   ++   ++   ++---++   ++---++   ++   ++---++   +
|   ||                  ||   ||             ||   ||   ||   ||   ||        ||        ||   ||        |
|   ||                  ||   ||             ||   ||   ||   ||   ||        ||        ||   ||        |
|   ||                  ||   ||             ||   ||   ||   ||   ||        ||        ||   ||        |
+   ++---++---++   ++   ++---++---++   ++   ++---++   ++---++   ++   ++   ++   ++---++---++   ++   +
+   ++---++---++   ++   ++---++---++   ++   ++---++   ++---++   ++   ++   ++   ++---++---++   ++   +
|   ||             ||   ||             ||   ||        ||        ||   ||   ||   ||             ||   |
|   ||             ||   ||             ||   ||        ||        ||   ||   ||   ||             ||   |
|   ||             ||   ||             ||   ||        ||        ||   ||   ||   ||             ||   |
+---++---++   ++   ++---++   ++---++   ++   ++---++---++   ++   ++---++   ++---++---++   ++   ++   +
+---++---++   ++   ++---++   ++---++   ++   ++---++---++   ++   ++---++   ++---++---++   ++   ++   +
|             ||   ||        ||        ||   ||             ||   ||        ||             ||   ||   |
|             ||   ||        ||        ||   ||             ||   ||        ||             ||   ||   |
|             ||   ||        ||        ||   ||             ||   ||        ||             ||   ||   |
+---++---++---++---++---++---++   ++   ++   ++---++   ++   ++---++---++---++---++   ++---++   ++   +
+---++---++---++---++---++---++   ++   ++   ++---++   ++   ++---++---++---++---++   ++---++   ++   +
|                                 ||   ||   ||        ||   ||                       ||        ||   |
|                                 ||   ||   ||        ||   ||                       ||        ||   |
|                                 ||   ||   ||        ||   ||                       ||        ||   |
+   ++---++---++---++---++   ++---++   ++   ++---++   ++---++   ++   ++   ++   ++---++---++---++   +
+   ++---++---++---++---++   ++---++   ++   ++---++   ++---++   ++   ++   ++   ++---++---++---++   +
|   ||                       ||        ||   ||        ||        ||   ||   ||   ||                  |
|   ||                       ||        ||   ||        ||        ||   ||   ||   ||                  |
|   ||                       ||        ||   ||        ||        ||   ||   ||   ||                  |
+   ++   ++   ++   ++---++---++---++   ++---++---++---++   ++---++   ++   ++---++---++   ++---++   +
+   ++   ++   ++   ++---++---++---++   ++---++---++---++   ++---++   ++   ++---++---++   ++---++   +
|   ||   ||   ||   ||                  ||                  ||        ||   ||             ||        |
|   ||   ||   ||   ||                  ||                  ||        ||   ||             ||        |
|   ||   ||   ||   ||                  ||                  ||        ||   ||             ||        |
+   ++---++   ++---++---++---++---++---++---++---++   ++   ++   ++   ++---++   ++---++   ++---++   +
+   ++---++   ++---++---++---++---++---++---++---++   ++   ++   ++   ++---++   ++---++   ++---++   +
|   ||        ||                                      ||   ||   ||   ||        ||        ||        |
|   ||        ||                                      ||   ||   ||   ||        ||        ||        |
|   ||        ||                                      ||   ||   ||   ||        ||        ||        |
+---++---++---++---++---++---++---++---++---++---++---++---++---++---++---++---++---++---++---++---+

bash$ 
```
