%%%-------------------------------------------------------------------
%%% @author Benjamin Adams
%%% @copyright (C) 2017, MessageMap.io
%%% @doc
%%%  This file is for removing user sesssions
%%% @end
%%% Created : 16. Aug 2017
%%%-------------------------------------------------------------------
-module(logout_handler).

-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {}).

init(_, Req, _Opts) ->
  {ok, Req, #state{}}.

handle(Req, State=#state{}) ->
  Req2 = cowboy_req:set_resp_cookie(<<"MessageMappAuth">>,  <<>>, [
      {max_age, 0}
    ], Req),
  {ok, Req3} = cowboy_req:reply(200, tools:resp_headers(),
    [],
    Req2),
  {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
  ok.
