#----------------------------------------------------------------------
# a basic makefile for the maze project
#----------------------------------------------------------------------

ERLC	= erlc
ERL	= erl

ebin/%.beam: src/%.erl
	$(ERLC) -W -o ebin -I include -v $<

all: compile

compile:	ebin/maze.app 			\
		ebin/maze_example.beam		\
		ebin/maze_util.beam		\
		ebin/maze_binary_tree.beam	\
		ebin/maze_sidewinder.beam	\
		ebin/maze.beam			\
		ebin/maze_app.beam		\
		ebin/maze_sup.beam		\
		ebin/maze_srv.beam		\
		ebin/maze_grid.beam		\
		ebin/maze_grid_sup.beam		\
		ebin/maze_grid_srv.beam		\
		ebin/maze_cell.beam		\
		ebin/maze_cell_sup.beam		\
		ebin/maze_cell_srv.beam

# temporary hack, need better solution
ebin/maze.app: src/maze.app.in
	sed -e 's/%%VSN%%/0.0.1/g' <$< >$@

ebin/maze_example.beam:			src/maze_example.erl		#include/maze.hrl
ebin/maze.beam:				src/maze.erl			#include/maze.hrl
ebin/maze_app.beam:			src/maze_app.erl		#include/maze.hrl
ebin/maze_sup.beam:			src/maze_sup.erl		#include/maze.hrl
ebin/maze_srv.beam:			src/maze_srv.erl		include/maze.hrl
ebin/maze_grid.beam:			src/maze_grid.erl		#include/maze.hrl
ebin/maze_grid_sup.beam:		src/maze_grid_sup.erl		#include/maze.hrl
ebin/maze_grid_srv.beam:		src/maze_grid_srv.erl		include/maze.hrl
ebin/maze_cell.beam:			src/maze_cell.erl		#include/maze.hrl
ebin/maze_cell_sup.beam:		src/maze_cell_sup.erl		#include/maze.hrl
ebin/maze_cell_srv.beam:		src/maze_cell_srv.erl		include/maze.hrl
ebin/maze_util.beam:			src/maze_util.erl		#include/maze.hrl
ebin/maze_binary_tree.beam:		src/maze_binary_tree.erl	#include/maze.hrl
ebin/maze_sidewinder.beam:		src/maze_sidewinder.erl		#include/maze.hrl

# normal
run: compile
	$(ERL) -pa ebin -s maze_example start

# just a convenience target
run-10-20: compile
	$(ERL) -pa ebin -s maze_example start -gridranks 10 20

# use short name
run-sname: compile
	$(ERL) -sname mfp -pa ebin -s maze_example start

# use long name
run-name: compile
	$(ERL) -name mfp -pa ebin -s maze_example start

# cleanup section
clean:
	find . -type f -name '*~' -execdir rm -f "{}" \;
	find . -type f -name 'erl_crash.dump' -execdir rm -f "{}" \;

mrproper: clean
	rm -f ebin/*.beam
	rm -f ebin/*.app

#----------------------------------------------------------------------
# End Of File
#----------------------------------------------------------------------