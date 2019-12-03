%%%-------------------------------------------------------------------
%%% @author Benjamin Adams
%%% @copyright (C) 2017, MessageMap.io
%%% @doc
%%%  This file is for auto building script for deployments
%%% @end
%%% Created : 12/14/2017
%%%-------------------------------------------------------------------
-module(deployed).

-export([rebuild/0, backupdb/0, pullConfig/0]).

-define(MNESIA_BK, "/tmp/messageMap").

%%Fix Later
pullConfig() ->
  {ok,{_,_,E_ip}} = httpc:request("https://api.ipify.org"),
  tools:log("info", io_lib:format("External IP: ~p~n", [E_ip])),
  Eip = re:replace(E_ip,"\"","",[{return,list}]),
  tools:log("info", io_lib:format("Using IP to custom setup application~n", [])),
  % TODO: change address to messagemap and use authentication
  %{ok, {_,_,ConfigPull}} = httpc:request("http://localhost/clientConfig/~s", [Eip]),
  { Eip }. %ConfigPull}.

rebuild() ->
  {ok, SrcFiles} = file:list_dir('./src'),
  lists:foreach(fun(F) ->
                      tools:log("info", io_lib:format("Compiling File: ~p~n", [
                        erlang:list_to_atom(
                          lists:nth(1, string:tokens(F, "."))
                        )
                      ])),
                      compile:file(erlang:list_to_atom(lists:nth(1, string:tokens(F, "."))))
              end, SrcFiles).

%TODO: Start using this as a normal backup
% -- Top Application Plus pay also backup db to cloud
% -- Support Restore also to prompt to new env
backupdb() ->
  {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_datetime(os:timestamp()),
  StrTime = lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w",[Year,Month,Day,Hour,Minute,Second])),
  file:make_dir(?MNESIA_BK),
  mnesia:backup(io_lib:format("~s/messageMap_~s_db.bk", [?MNESIA_BK,StrTime])).
  %TODO: Runscript backup to google cloud
  % gs://messagemapdbbk
