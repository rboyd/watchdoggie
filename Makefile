
#.PHONY: test

all:
	erl -make

test: all
	cd test ; ./test.escript

#run_echo: all
#	erl -s echo_server
