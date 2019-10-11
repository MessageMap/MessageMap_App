%%%-------------------------------------------------------------------
%%% @author Ben Adams
%%% @copyright (C) 2017, MessageMapp.io
%%% @doc
%%%   Module is used for a collection of Random Tools to use in the project
%%% @end
%%% Created : 09. Aug 2017
%%%-------------------------------------------------------------------
-module(tools).

-export([log/2, pull_global_stats/0, resp_headers/0, integer_check/1, verifyAuthAdmin/1, verifyAuth/1, version/0, convertDateTime/1]).

-define(server, "MessageMap.io").
-define(version, "0.0.1 Alpha").
-define(hostname, os:getenv("MM_HOSTNAME")).

resp_headers()->
  [
    {<<"content-type">>, <<"application/json">>},
    {<<"company">>, <<"MessageMap.io">>},
    {<<"server">>, <<?server>>},
    {<<"version">>, <<?version>>}
  ].

log(Level="info", MsgRaw)->
  Msg = erlang:binary_to_list(erlang:iolist_to_binary(MsgRaw)),
  MsgWrite = erlang:binary_to_list(erlang:iolist_to_binary(io_lib:format(" { 'host': '~p', 'level': '~p', 'msg': ~p }", [?hostname, Level, Msg]))),
  os:cmd("logger -t msgmap " ++ MsgWrite).

verifyAuth(Req) ->
  tools:log("info", io_lib:format("Pulling user Token", [])),
  { AuthValue, Req2 } = cowboy_req:cookie(<<"MessageMapAuth">>, Req, <<"Bad">>),
  tools:log("info", io_lib:format("Cookie: ~p", [AuthValue])),
  if
    AuthValue == <<"Bad">> ->
      { AuthValue, Req2 };
    true ->
      Claims = encryption:ewtDecode(AuthValue),
      tools:log("info", io_lib:format("Claims: ~p~n", [Claims])),
      { Claims, Req2 }
  end.

verifyAuthAdmin(Req) ->
  tools:log("info", io_lib:format("Pull Admin Token", [])),
  { AuthValue, Req2 } = cowboy_req:cookie(<<"AdminMessageMapAuth">>, Req, <<"Bad">>),
  tools:log("info", io_lib:format("Admin Cookie: ~p", [AuthValue])),
  if
    AuthValue == <<"Bad">> ->
      { AuthValue, req2 };
    true ->
      Claims = encryption:adminEwtDecode(AuthValue),
      tools:log("info", io_lib:format("Admin Claims: ~p~n", [Claims])),
      { Claims, Req2}
  end.

integer_check({ Num, <<>>}) ->
  Num;
integer_check(_) ->
  10.

version() ->
  "{\"Server\":\"Welcome to Message Map\",
    \"Information\": \"Read More at Message Map (MessageMap.io) \",
    \"version\": \"" ++ ?version ++ "\"}".

convertDateTime(Now) ->
    {{YY, MM, DD}, {Hour, Min, Sec}} = Now,
    lists:concat([MM, "-", DD, "-", YY, " ", Hour, ":", Min, ":", Sec]).

pull_global_stats() ->
    Apps = database:getAllAppDB(),
    Result = lists:map(fun(A) ->
        {_, Id, Name, _, _, _, _, _} = A,
        Tbl = database:check_dyn_table(Id),
        [{_,_,Published}] = mnesia:dirty_read({counter_published, Tbl}),
  	[{_,_,Consumed}] = mnesia:dirty_read({counter_consumed, Tbl}),
	{
	  {<<"id">>, Id},
          {<<"name">>, Name},
          {<<"queue">>, mnesia:table_info(Tbl, size)},
          {<<"published">>,  Published},
          {<<"consumed">>,  Consumed}
        }
        end, Apps),
    Result.
