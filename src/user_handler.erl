%%%-------------------------------------------------------------------
%%% @author Benjamin Adams
%%% @copyright (C) 2017, Ethan Solutions
%%% @doc
%%%  Return to the user contents of their token
%%% @end
%%% Created : 8. Sept 2017
%%%-------------------------------------------------------------------
-module(user_handler).

-export([init/2]).

init(Req, Opts) ->
  Method = cowboy_req:method(Req),
  Result = processRequest(Method, Req),
  { ok, ReqFinal } = cowboy_req:reply(200, tools:resp_headers(),
      Result,
      Req),
  {ok, ReqFinal, Opts}.

% Internal functions
processRequest(<<"POST">>, Req) ->
  {ok, Body, _} = cowboy_req:read_urlencoded_body(Req),
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
