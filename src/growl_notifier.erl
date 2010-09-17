% growl_notifier.erl -- (c) Robert Boyd 2010
%
%		Sends build results to growl via Port command with osascript.

-module(growl_notifier).
-export([register/0, notify/3]).


osascript(Script) ->
	Port = open_port({spawn, "osascript"}, []),
	port_command(Port, Script),
	port_close(Port).
	
register() ->
	osascript("tell application \"GrowlHelperApp\"
		set the allNotificationsList to {\"Build Results\"}
		set the enabledNotificationsList to {\"Build Results\"}

		register as application \"watchdoggie\" all notifications allNotificationsList default notifications enabledNotificationsList icon of application \"Script Editor\"
	end tell").

notify(_Name, Title, Description) ->
	osascript("tell application \"GrowlHelperApp\"
		notify with name \"Build Results\" title \"" ++ Title ++ "\" description \"" ++ Description ++ "\" application name \"watchdoggie\"
	end tell").
	
