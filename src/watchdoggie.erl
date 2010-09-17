-module(watchdoggie).
-export([go/0]).

go() ->
	io:format("watchdoggie service started.~n"),
	watcher:monitor(filename:absname("") ++ "/src").