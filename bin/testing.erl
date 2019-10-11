#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname factorial -mnesia debug verbose

-define(ME, filename:basename(escript:script_name())).

main([String]) ->
    try
        N = list_to_integer(String),
        F = fac(N),
        io:format("factorial ~w = ~w\n", [N,F])
    catch
        _:_ ->
            usage()
    end;
main(_) ->
    net_kernel:start([my_name(), longnames]),
    erlang:set_cookie(my_name(), list_to_atom("test")),
    %Node = "localhost",
    net_kernel:connect_node('dev@development'),
    R = rpc:call('dev@development', tools, resp_headers, []),
    %R = rpc:multicall(nodes(), tools, resp_headers, []),
    io:format("R~p~n", [R]),
    usage().

usage() ->
    io:format("usage: factorial integer\n"),
    halt(1).

fac(0) -> 1;
fac(N) -> N * fac(N-1).

my_name() ->
    Localhost = net_adm:localhost(),
    list_to_atom(?ME ++ "@" ++ Localhost).
