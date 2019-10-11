%%%-------------------------------------------------------------------
%%% @author Benjamin Adams
%%% @copyright (C) 2017, MessageMap.io
%%% @doc
%%%  Administration Add User to system
%%% @end
%%% Created : 8. Sept 2017
%%%-------------------------------------------------------------------
-module(admin_add_user_handler).

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
processRequest(<<"POST">>, _, Req) ->
  {ok, Body, _} = cowboy_req:body_qs(Req),
  {_, Username} = lists:keyfind(<<"user">>, 1, Body),
  {_, Password} = lists:keyfind(<<"password">>, 1, Body),
  tools:log("info", io_lib:format("New User: ~p~n", [Username])),
  if
    Username =:= 1 ->
      jiffy:encode(#{ "Error" => "Bad Username"});
    Password =:= 1 ->
      jiffy:encode(#{ "Error" => "Bad Password"});
    true ->
      U = lists:flatten(io_lib:format("~s", [Username])),
      P = lists:flatten(io_lib:format("~s", [Password])),
      database:storeDB(U, U, ["Admin"], P),
      jiffy:encode(#{ update => true })
  end.
