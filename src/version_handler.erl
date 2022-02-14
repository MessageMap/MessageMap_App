%%%-------------------------------------------------------------------
%%% @author Ben Adams - Ben@MessageMap.IO
%%% @copyright (C) 2017-2022, MessageMap LLC
%%% @doc
%%%  This file is to track the version number deployed for end user lookup
%%% @end
%%% Created : 09. Aug 2017
%%%-------------------------------------------------------------------
-module(version_handler).

-export([init/2]).

init(Req, Opts) ->
    Req2 = cowboy_req:reply(
        200,
        tools:resp_headers(),
        tools:version(),
        Req
    ),
    {ok, Req2, Opts}.
