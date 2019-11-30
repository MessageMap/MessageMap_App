%%%-------------------------------------------------------------------
%%% @author Benjamin Adams
%%% @copyright (C) 2017, MessageMap.io
%%% @doc
%%%  Return to the user contents of their token
%%% @end
%%% Created : 8. Sept 2017
%%%-------------------------------------------------------------------
-module(me_handler).

-export([init/2]).

init(Req, Opts) ->
  { Claims, Req2 } = tools:verifyAuth(Req),
  ReqFinal = cowboy_req:reply(200, tools:resp_headers(),
      jiffy:encode(Claims),
      Req2),
  { ok, ReqFinal, Opts}.
