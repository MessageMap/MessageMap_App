%%%-------------------------------------------------------------------
%%% @author Benjamin Adams
%%% @copyright (C) 2017, MessageMap.io
%%% @doc
%%%  This file is for authentication for users
%%% @end
%%% Created : 16. Aug 2017
%%%-------------------------------------------------------------------
-module(auth_handler).

-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

%% Define constant variables
-define(DOMAIN, os:getenv("MM_HOSTNAME")).

-record(state, {}).

init(_, Req, _Opts) ->
  {ok, Req, #state{}}.

handle(Req, State=#state{}) ->
  {ok, BodyQs, Req0} = cowboy_req:body_qs(Req),
  Email = proplists:get_value(<<"username">>, BodyQs),
  Password = proplists:get_value(<<"password">>, BodyQs),
  { ResultOfLogin, EwtToken } = database:login(binary_to_list(Email), binary_to_list(Password)),
  Result = jiffy:encode(io_lib:format("{ \"request\": ~p }", [ResultOfLogin])),
  if
    ResultOfLogin ->
      Req2 = cowboy_req:set_resp_cookie(<<"MessageMapAuth">>, EwtToken, [{domain, ?DOMAIN}], Req0);
    true ->
      Req2 = Req0
  end,
  {ok, Req3} = cowboy_req:reply(200, tools:resp_headers(),
    jiffy:decode(Result),
    Req2),
  {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
  ok.
