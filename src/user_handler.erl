%%%-------------------------------------------------------------------
%%% @author Benjamin Adams
%%% @copyright (C) 2017, Ethan Solutions
%%% @doc
%%%  Return to the user contents of their token
%%% @end
%%% Created : 8. Sept 2017
%%%-------------------------------------------------------------------
-module(user_handler).

-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {}).

init(_, Req, _Opts) ->
  {ok, Req, #state{}}.

handle(Req, State=#state{}) ->
  { Method, _ } = cowboy_req:method(Req),
  Result = processRequest(Method, Req),
  { ok, ReqFinal } = cowboy_req:reply(200, tools:resp_headers(),
      Result,
      Req),
  {ok, ReqFinal, State}.

terminate(_Reason, _Req, _State) ->
  ok.

% Internal functions
processRequest(<<"POST">>, Req) ->
  {ok, Body, _} = cowboy_req:body_qs(Req),
  {_, Username} = lists:keyfind(<<"username">>, 1, Body),
  {_, Password} = lists:keyfind(<<"password">>, 1, Body),
  database:saveDB(binary:bin_to_list(Username), binary:bin_to_list(Username), [], binary:bin_to_list(Password)),
  Result = buildResponse(binary:bin_to_list(Username)),
  jiffy:encode(Result).

%% Internal functions
buildResponse(Data) ->
  #{
    username => Data
  }.
