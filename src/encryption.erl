%%%-------------------------------------------------------------------
%%% @author Ben Adams
%%% @copyright (C) 2017, MessageMap.io
%%% @doc
%%% Module will be used for encryption
%%% @end
%%% Created : 1. Sept 2017
%%%-------------------------------------------------------------------
-module(encryption).

-export([create/1, ewtCreate/3, adminEwtDecode/1, ewtDecode/1, oauthCreate/2, validate/2]).

-define(saltround, 10).
-define(ewtKey, os:getenv("MM_ENCRYPTION")).
-define(ewtAdminKey, "EthanAdams_4292").

create(Password) ->
 os:cmd(io_lib:format("openssl passwd -1 -salt ~s ~s", [?ewtKey, Password])).

ewtCreate(Name, Email, Permissions) ->
  Expiration = calendar:datetime_to_gregorian_seconds(calendar:universal_time()) + 1*60*60,
  Claims = #{user => Email, name => Name, roles => [Permissions] },
  ewt:token(Expiration, Claims, ?ewtKey, sha256).

ewtDecode(Ewt) ->
  Value = ewt:claims(lists:nth(2, string:tokens(Ewt, " ")), ?ewtKey),
  pullDecodeResponse(Value).

adminEwtDecode(Ewt) ->
  Value = ewt:claims(Ewt, ?ewtAdminKey),
  io:format("JWt: ~p~n", [Value]),
  pullDecodeResponse(Value).

validate(Password, ExistingHash) ->
  ProvidedHash = os:cmd(io_lib:format("openssl passwd -1 -salt ~s ~s", [?ewtKey, Password])),
  case ProvidedHash == ExistingHash of
    true -> success;
    false -> fail
  end.

oauthCreate(Claims, Exp) ->
  Expiration = calendar:datetime_to_gregorian_seconds(calendar:universal_time()) + Exp,
  ewt:token(Expiration, Claims, ?ewtKey, sha256).

%%%% Internal Functions
pullDecodeResponse(bad) ->
  fail;
pullDecodeResponse(expired) ->
  fail;
pullDecodeResponse({reason,{badmatch,expired}}) ->
  fail;
pullDecodeResponse(V) ->
  {ok, Response} = V,
  Response.
