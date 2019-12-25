%%%-------------------------------------------------------------------
%%% @author Benjamin Adams
%%% @copyright (C) 2019, MessageMap LLC
%%% @doc
%%%  Validate Encryption Key
%%% @end
%%% Created : 24. Dec 2019
%%%-------------------------------------------------------------------
-module(validate_encryption).

-export([init/2]).

init(Req, Opts) ->
  { Claims, Req2 } = tools:verifyAuth(Req),
  % redirect if Claims = Bad
  Method = cowboy_req:method(Req),
  Result = processRequest(Method, Claims, Req),
  ReqFinal = cowboy_req:reply(200, tools:resp_headers(),
      Result,
      Req2),
  {ok, ReqFinal, Opts}.

% Internal functions
processRequest(<<"POST">>, _, Req) ->
  {ok, Body, _} = cowboy_req:read_urlencoded_body(Req),
  {_, EncryptString } = lists:keyfind(<<"encrypt">>, 1, Body),
  try encryption:msgEncryption("Testing", EncryptString) of
    _ ->
      jiffy:encode(#{ result => good })
    catch
      _:_ ->
        jiffy:encode(#{ result => bad })
  end.
