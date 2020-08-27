#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname factorial -mnesia debug verbose

main(_) ->
  io:format("Starting to Parse db Backup~n"),
  Filename = "/tmp/db_app_backup.db",
  {ok, Result} = file:read_file(Filename),
  DB_String = erlang:binary_to_term(base64:decode(Result)),
  io:format("DB String: ~p~n", [DB_String]).
