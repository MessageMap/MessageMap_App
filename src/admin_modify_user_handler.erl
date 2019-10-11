%%%-------------------------------------------------------------------
%%% @author Benjamin Adams
%%% @copyright (C) 2019, MessageMap.io
%%% @doc
%%%  Administration Modify User to system
%%% @end
%%% Created : 3. Sept 2019
%%%-------------------------------------------------------------------
-module(admin_modify_user_handler).

-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {}).

init(_, Req, _Opts) ->
  {ok, Req, #state{}}.

handle(Req, State=#state{}) ->
  % TODO: Add Claims ladder
  % { Claims, Req2 } = tools:verifyAuthAdmin(Req),
  { Method, _ } = cowboy_req:method(Req),
  %io:format("Claims: ~p~n", [Claims]),
  Result = processRequest(Method, "REPLACE_CLAIMS", Req),
  io:format("Result: ~p~n", [Result]),
  { ok, ReqFinal } = cowboy_req:reply(200, tools:resp_headers(),
      Result,
      Req),
  {ok, ReqFinal, State}.

terminate(_Reason, _Req, _State) ->
  ok.

% Internal functions
processRequest(<<"PUT">>, _, Req) ->
  {ok, Body, _} = cowboy_req:body_qs(Req),
  {_, Username} = lists:keyfind(<<"user">>, 1, Body),
  {_, Password} = lists:keyfind(<<"password">>, 1, Body),
  tools:log("info", io_lib:format("Modify User: ~p~n", [Username])),
  if
    Username =:= 1 ->
      jiffy:encode(#{ "Error" => "Bad Username"});
    Password =:= 1 ->
      jiffy:encode(#{ "Error" => "Bad Password"});
    true ->
      U = lists:flatten(io_lib:format("~s", [Username])),
      P = lists:flatten(io_lib:format("~s", [Password])),
      database:deleteDBUser(U),
      database:storeDB(U, U, ["Admin"], P),
      jiffy:encode(#{ update => true })
  end.
