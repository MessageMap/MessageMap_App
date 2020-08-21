%%%-------------------------------------------------------------------
%%% @author Ben Adams
%%% @copyright (C) 2017, MessageMapp.io
%%% @doc
%%%   Module is used for a collection of Random Tools to use in the project
%%% @end
%%% Created : 09. Aug 2017
%%%-------------------------------------------------------------------
-module(tools).

-export([log/2, osStats/0, pull_global_stats/0, resp_headers/0, integer_check/1, verifyAuthAdmin/1, verifyAuth/1, version/0, convertDateTime/1, pullAppLimit/0]).

-define(server, "MessageMap.io").
-define(version, "0.0.1 Beta").
-define(hostname, os:getenv("MM_HOSTNAME")).
-define(loggly, "http://logs-01.loggly.com/inputs/a6f62204-c858-423f-8cf1-725f9149cd30/tag/http/").

resp_headers()->
  #{
    <<"content-type">> => <<"application/json">>,
    <<"company">> => <<"MessageMap.io">>,
    <<"server">> => <<?server>>,
    <<"version">> => <<?version>>
  }.

osStats() ->
  [{_,_,Disk},_] = disksup:get_disk_data(),
  Cpu = cpu_sup:avg1(),
  Mem = memsup:get_sysmem_high_watermark(),
  Usage = #{
    "cpu" => Cpu,
    "disk" => Disk,
    "mem" => Mem
  },
  Limit = ((Cpu < 400) and (Disk < 90) and (Mem < 90)),
  %tools:log("info", io_lib:format("CPU LOAD: ~p~n", [{Limit, Usage}])),
  { Limit, Usage }.

log(Level="info", MsgRaw)->
  Msg = erlang:binary_to_list(erlang:iolist_to_binary(MsgRaw)),
  MsgWrite = erlang:binary_to_list(erlang:iolist_to_binary(io_lib:format('{ "host": "~s", "level": "~s", "msg": ~p }', [?hostname, Level, Msg]))),
  % curl -H "content-type:application/json" -d '{ "message" : "hello" }' http://logs-01.loggly.com/inputs/a6f62204-c858-423f-8cf1-725f9149cd30/tag/http/
  %TODO SETUP IF IN Dev by hostname
  %os:cmd("logger -t msgmap " ++ MsgWrite).
  httpc:request(post, {?loggly, [], "application/json", MsgWrite}, [], []).

verifyAuth(Req) ->
  #{messageMapAuth := AuthValue } = cowboy_req:match_cookies([{messageMapAuth, [], <<"Bad">>}], Req),
  if
    AuthValue == <<"Bad">> ->
      { AuthValue, Req };
    true ->
      Claims = encryption:ewtDecode(AuthValue),
      { Claims, Req }
  end.

verifyAuthAdmin(Req) ->
  #{adminMessageMapAuth := AuthValue } = cowboy_req:match_cookies([{adminMessageMapAuth, [], <<"Bad">>}], Req),
  if
    AuthValue == <<"Bad">> ->
      { AuthValue, Req };
    true ->
      Claims = encryption:adminEwtDecode(AuthValue),
      { Claims, Req }
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

pullAppLimit() ->
  {_, Size, _} = lists:nth(2, disksup:get_disk_data()),
  if
     Size > 640000000 ->
       200;
     Size > 300000000 ->
       100;
     Size > 150000000 ->
       50;
     true ->
       20
  end.
