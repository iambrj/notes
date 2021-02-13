-module(number_analyser).
-export([start/0, server/1,add_number/2, analyse/1]).

start() ->
    register(number_analyser, spawn(number_analyser, server, [nil])).

add_number(Seq, Dest) ->
    request({add_number, Seq, Dest}).

analyse(Seq) ->
    request({add_number, Seq}).

request(Req) ->
    number_analyser ! {self(), Req},
    receive
        {number_analyser, Reply} ->
            Reply
    end.

server(AnalTable) ->
    receive
        {From, {analyse, Seq}} ->
            Result = lookup(Seq, AnalTable),
            From ! {number_analyser, ack},
            server(AnalTable);
        {From, {add_number, Seq, Dest}} ->
            From ! {number_analyser, ack},
            server(insert(Seq, Dest, AnalTable))
    end.
