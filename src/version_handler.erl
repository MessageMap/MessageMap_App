%%%-------------------------------------------------------------------
%%% @author Benjamin Adams
%%% @copyright (C) 2017, MessageMap.io
%%% @doc
%%%  This file is to track the version number deployed for end user lookup
%%% @end
%%% Created : 09. Aug 2017
%%%-------------------------------------------------------------------
-module(version_handler).

-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {}).

init(_, Req, _Opts) ->
  {ok, Req, #state{}}.

handle(Req, State=#state{}) ->
  {ok, Req2} = cowboy_req:reply(200, tools:resp_headers(),
    tools:version(),
    Req),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.
