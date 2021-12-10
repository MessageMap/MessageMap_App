%%%-------------------------------------------------------------------
%%% @author Ben Adams
%%% @copyright (C) 2017, MessageMapp.io
%%% @doc
%%%   Module is used for a collection of Random Tools to use in the project
%%% @end
%%% Created : 09. Aug 2017
%%%-------------------------------------------------------------------
-module(tools).

-export([log/2]).
-export([configFile/1]).
-export([generate_string/1]).
-export([pull_global_stats/0]).
-export([resp_headers/0]).
-export([integer_check/1, verifyAuth/1, version/0, convertDateTime/1]).
-export([requireAdmin/2]).
-export([send_data/1]).

-define(server, "MessageMap.io").
-define(version, "0.1.0").
-define(statsUrl, "https://us-south.functions.appdomain.cloud/api/v1/web/ben%40messagemap.io_dev/default/ApplicationStats.json").

resp_headers()->
  #{
    <<"content-type">> => <<"application/json">>,
    <<"company">> => <<"MessageMap.io">>,
    <<"server">> => <<?server>>,
    <<"connection">> => <<"close">>,
    <<"version">> => <<?version>>
  }.

generate_string(Length) ->
  AllowedChars = "qwertyuioplkjhgfdsdsazxcvbnm1234567890QWERTYUIOPLKJHGFDSAZXCVBNM,.?;[]{}=+",
  lists:foldl(fun(_, Acc) ->
          [lists:nth(rand:uniform(length(AllowedChars)),
                     AllowedChars)]
              ++ Acc
  end, [], lists:seq(1, Length)).

configFile("dataNodes") ->
  [node()];
configFile("nodes") ->
  [node()];
configFile(FieldName) ->
  Config = "/etc/messagemap/messagemap.conf",
  FileFound = filelib:is_regular(Config),
  if
    FileFound == false ->
      log("ERROR", io:format("No Configuration File: ~p", [Config])),
      erlang:error("No Configuration File: ~p", [Config]);
    true ->
      true
  end,
  erlang:binary_to_list(maps:get(FieldName, readlines(Config), <<>>)).

local_ip_v4() ->
    {ok, Addrs} = inet:getifaddrs(),
    hd([
         Addr || {_, Opts} <- Addrs, {addr, Addr} <- Opts,
         size(Addr) == 4, Addr =/= {127,0,0,1}
    ]).

send_data("False") ->
  log("info", "Data will not be sent over");
send_data(_) ->
  log("info", "Starting Send Stats Data"),
  Data = #{
    <<"_id">> => erlang:list_to_binary(lists:concat(string:replace(erlang:atom_to_list(node()), "@", "-"))++"_"++lists:concat(erlang:tuple_to_list(local_ip_v4()))),
    <<"msgmap">> => ws_handler:pullData()
  },
  httpc:request(post, {?statsUrl, [], "application/json", jiffy:encode(Data)},[{ssl, [{verify, verify_none}]}],[]),
  log("info", "Done Sending Data").

requireAdmin(true, Claims) ->
  validateAdmin(maps:get(<<"roles">>, Claims));
requireAdmin(_, _) ->
  <<"Bad">>.

validateAdmin([<<"Admin">>]) ->
  true;
validateAdmin(_) ->
  <<"Bad">>.

log(Level="info", MsgRaw)->
  Msg = erlang:binary_to_list(erlang:iolist_to_binary(MsgRaw)),
  MsgWrite = erlang:binary_to_list(erlang:iolist_to_binary(io_lib:format('["~s"] - ~p~n', [ Level, Msg]))),
  {ok, L} = file:open(configFile("log_dir") ++"/messagemap.log", [append]),
  io:format(L, "~s", [MsgWrite]).

verifyAuth(Req) ->
  Method = cowboy_req:method(Req),
  #{messageMapAuth := AuthValue } = cowboy_req:match_cookies([{messageMapAuth, [], <<"Bad">>}], Req),
  if
    AuthValue == <<"Bad">> ->
      { AuthValue, Req };
    true ->
      Claims = encryption:ewtDecode(AuthValue),
      Permissions = maps:get(<<"roles">>, Claims),
      if 
        ((Permissions == [<<"Read">>]) andalso (Method /= [<<"GET">>] )) ->
          { <<"Bad">>, Req};
        true ->
          { Claims, Req }
      end
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

readlines(FileName) ->
    Result = maps:new(),
    {ok, Device} = file:open(FileName, [read]),
    Final = try get_all_lines(Device, Result)
      after file:close(Device)
    end,
    Final.

get_all_lines(Device, R) ->
    case io:get_line(Device, "") of
        eof  -> R;
        Line ->
          Tokens = string:tokens(Line, "="),
          if
            length(Tokens) == 2 ->
              get_all_lines(
                Device,
                maps:put(
                  lists:nth(1, Tokens),
                  lists:nth(1, re:replace(lists:nth(2, Tokens), "[\r\n]", [])),
                  R
                ));
            true ->
              get_all_lines(Device, R)
          end
    end.
