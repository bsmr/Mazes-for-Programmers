#
# a basic makefile for the maze project
#

ERLC	= erlc
ERL	= erl

ebin/%.beam: src/%.erl
	$(ERLC) -W -o ebin -I include -v $<

all: compile

compile:	ebin/maze.app 		\
		ebin/maze.beam		\
		ebin/maze_app.beam	\
		ebin/maze_sup.beam	\
		ebin/maze_srv.beam	\
		ebin/maze_grid_sup.beam	\
		ebin/maze_grid_srv.beam	\
		ebin/maze_cell_sup.beam	\
		ebin/maze_cell_srv.beam

ebin/maze.app: src/maze.app.in
	sed -e 's/VSN/0.0.0/g' <$< >$@

ebin/maze.beam: src/maze.erl
ebin/maze_app.beam: src/maze_app.erl
ebin/maze_sup.beam: src/maze_sup.erl
ebin/maze_srv.beam: src/maze_srv.erl
ebin/maze_grid_sup.beam: src/maze_grid_sup.erl
ebin/maze_grid_srv.beam: src/maze_grid_srv.erl
ebin/maze_cell_sup.beam: src/maze_cell_sup.erl
ebin/maze_cell_srv.beam: src/maze_cell_srv.erl

run: compile
	$(ERL) -pa ebin -s maze start

clean:
	find . -type f -name '*~' -execdir rm -f "{}" \;
	find . -type f -name 'erl_crash.dump' -execdir rm -f "{}" \;

mrproper: clean
	rm -f ebin/*.beam
	rm -f ebin/*.app

#
# End Of File
#