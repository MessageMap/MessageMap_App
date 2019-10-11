%%%-------------------------------------------------------------------
%%% @author Benjamin Adams
%%% @copyright (C) 2017, MessageMap.io
%%% @doc
%%%  Return to the user contents of their token
%%% @end
%%% Created : 8. Sept 2017
%%%-------------------------------------------------------------------
-module(me_handler).

-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {}).

init(_, Req, _Opts) ->
  {ok, Req, #state{}}.

handle(Req, State=#state{}) ->
  { Claims, Req2 } = tools:verifyAuth(Req),
  { ok, ReqFinal} = cowboy_req:reply(200, tools:resp_headers(),
      jiffy:encode(Claims),
      Req2),
  { ok, ReqFinal, State}.

terminate(_Reason, _Req, _State) ->
  ok.
