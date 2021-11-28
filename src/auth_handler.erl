%%%-------------------------------------------------------------------
%%% @author Benjamin Adams
%%% @copyright (C) 2017, MessageMap.io
%%% @doc
%%%  This file is for authentication for users
%%% @end
%%% Created : 16. Aug 2017
%%%-------------------------------------------------------------------
-module(auth_handler).

-export([init/2]).

init(Req, Opts) ->
  {ok, BodyQs, Req0} = cowboy_req:read_urlencoded_body(Req),
  Email = proplists:get_value(<<"username">>, BodyQs),
  Password = proplists:get_value(<<"password">>, BodyQs),
  { ResultOfLogin, EwtToken } = database:login(binary_to_list(Email), binary_to_list(Password)),
  Result = jiffy:decode(io_lib:format("{ \"request\": ~p }", [ResultOfLogin])),
  if
    ResultOfLogin ->
      Req2 = cowboy_req:set_resp_cookie(<<"messageMapAuth">>, EwtToken, Req0);
    true ->
      Req2 = Req0
  end,
  Req3 = cowboy_req:reply(200, tools:resp_headers(),
    jiffy:encode(Result),
    Req2),
  {ok, Req3, Opts}.
