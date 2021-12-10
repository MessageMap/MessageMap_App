%%%-------------------------------------------------------------------
%%% @author Ben Adams
%%% @copyright (C) 2017, MessageMap.io
%%% @doc
%%% Module Handle all startup and shutdown actions
%%% @end
%%% Created : 09. Aug 2017
%%%-------------------------------------------------------------------
-module(appmanager).

-export([start/0, stop/0]).

-define(cnf_cookie, tools:configFile("cookie")).
-define(send_data, tools:configFile("upload_stats")).

start() ->
  net_kernel:start([shell, shortnames]),
  erlang:set_cookie(node(), erlang:list_to_atom(?cnf_cookie)),
  tools:log("info", "Boot_Start - Startup Script Has begun"),
  database:init(),
  timer:sleep(1000),
  database_manager:init(),
  tools:log("info", "Boot_Start - Startup Script Has Finished"),
  tools:log("info", "Boot_Start - Welcome to MessageMap !!"),
  {ok, Version} = maps:find(<<"version">>, jiffy:decode(tools:version(), [return_maps])),
  tools:send_data(?send_data),
  timer:apply_interval(timer:hms(10,0,0), tools, send_data, [?send_data]),
  tools:log("info", io_lib:format("Boot_Start - Version: ~s", [erlang:binary_to_list(Version)])).

stop() ->
  tools:log("info", "Boot_Shutdown - Shutdown Script Has begun"),
  mnesia:stop(),
  tools:log("info", "Boot_Shutdown - Shutdown Script Has Finished").
