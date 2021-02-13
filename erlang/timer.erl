-module(timer).
-export([timeout/2, cancel/1, timer/3]).

timeout(Time, Alarm) ->
    spawn(timer, timer, [self(), Time, Alarm]).

timer(Pid, Time, Alarm) ->
    receive
        {Pid, cancel} ->
            true
        after Time ->
            Pid ! Alarm
    end.

cancel(Timer) ->
    Timer ! {self(), cancel}.
