% watchdoggie.erl -- (c) Robert Boyd 2010
%
%		Registers with Growl and starts a watcher to monitor for the src directory.

-module(watchdoggie).
-export([go/0]).

go() ->
	io:format("watchdoggie service started.~n"),
	growl_notifier:register(),
	watcher:monitor(filename:absname("") ++ "/src").