% watchdoggie.erl -- (c) Robert Boyd 2010
%
%		Watches a directory for modifications of src/*.erl source files.  Kicks off a build after updates.

-module(watcher).
-export([monitor/1, ends_with/2, get_files/1, make/0]).

-include_lib("kernel/include/file.hrl").

-define(CHECK_FREQUENCY, 2000).

-record(watcher_rec, {path, tree}).


% shamelessly jacked from http://github.com/hyperthunk/hamcrest-erlang/blob/master/src/hamcrest_matchers.erl
ends_with(X, Y) ->
	string:equal(string:right(Y, length(X)), X).
	

% builds a tree Key = file_info.mtime, Value = Filename
get_files(Path) ->
	{ok, Filenames} = file:list_dir(Path),
	
	Initialized = lists:foldl(
		fun(Filename, AccIn) ->
			{ok, FileInfo} = file:read_file_info(Path ++ "/" ++ Filename),
			IsSource = ends_with(".erl", Filename),
			case IsSource of
				true ->
					gb_trees:enter(FileInfo#file_info.mtime, Filename, AccIn);
				false ->
					AccIn
			end
		end,
		gb_trees:empty(), Filenames),
	#watcher_rec{path = Path, tree = Initialized}.

monitor(Path) ->
	WatcherRec = get_files(Path),
	Tree = WatcherRec#watcher_rec.tree,
	loop(WatcherRec, gb_trees:largest(Tree)).

loop(State, Largest) ->
	timer:sleep(?CHECK_FREQUENCY),
	Path = State#watcher_rec.path,
	Tree = (get_files(Path))#watcher_rec.tree,
	Latest = gb_trees:largest(Tree),
	case Latest > Largest of
		true ->
			make();
		false ->
			nop
	end,
	loop(State#watcher_rec{tree = Tree}, erlang:max(Largest, Latest)).
	
make() ->
	io:format("Building...~n"),
	{Time, Value} = timer:tc(os, cmd, ["make"]),
	io:format(Value),
	io:format("Finished in ~p sec~n~n", [Time/1000000]).