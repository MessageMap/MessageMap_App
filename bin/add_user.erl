#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname factorial -mnesia debug verbose


-define(COOKIE, "MessageMap123").
% database:storeDB("MessageMap", "info@messagemap.io", ["Admin"], "Demo99"),
main([Node]) ->
%  try
    {ok, _} = net_kernel:start(['adminrecover@127.0.0.1', shortnames]),
    erlang:set_cookie(node(), list_to_atom(?COOKIE)),
    case net_adm:ping(list_to_atom(Node)) of
      pong -> halt();
      pang -> halt(2)
%    end
%  catch _:Reason
%    io:format("~p~n", [Reason]),
%    halt(3)
  end.
  % Target = list_to_atom(Node),
  %process_flag(trap_exit, true),
  %Shell = user_drv:start(['ttl_sl -c -e', {Target,shell,start,[]}]),
  %true = erlang:link(Shell),
  %io:format("Grabbed a remote shell on ~p~n", [Target]),
  %receive
  %  {'EXIT', Shell, _} -> ok
  %end.
