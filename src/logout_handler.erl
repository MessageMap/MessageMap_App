%%%-------------------------------------------------------------------
%%% @author Benjamin Adams
%%% @copyright (C) 2017, MessageMap.io
%%% @doc
%%%  This file is for removing user sesssions
%%% @end
%%% Created : 16. Aug 2017
%%%-------------------------------------------------------------------
-module(logout_handler).

-export([init/2]).

init(Req, Opts) ->
  Req2 = cowboy_req:set_resp_cookie(<<"MessageMappAuth">>,  <<>>, [
      {max_age, 0}
    ], Req),
  {ok, Req3} = cowboy_req:reply(200, tools:resp_headers(),
    [],
    Req2),
  {ok, Req3, Opts}.
