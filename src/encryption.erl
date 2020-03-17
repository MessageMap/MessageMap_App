%%%-------------------------------------------------------------------
%%% @author Ben Adams
%%% @copyright (C) 2017, MessageMap.io
%%% @doc
%%% Module will be used for encryption
%%% @end
%%% Created : 1. Sept 2017
%%%-------------------------------------------------------------------
-module(encryption).
-include_lib("public_key/include/public_key.hrl").

-export([create/1]).
-export([ewtCreate/3]).
-export([adminEwtDecode/1]).
-export([ewtDecode/1]).
-export([generatePass/1]).
-export([oauthCreate/2]).
-export([validate/2]).
-export([msgEncryption/2]).
-export([certRenew/0]).

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
  Value = ewt:claims(Ewt, ?ewtKey),
  pullDecodeResponse(Value).

adminEwtDecode(Ewt) ->
  Value = ewt:claims(Ewt, ?ewtAdminKey),
  pullDecodeResponse(Value).

validate(Password, ExistingHash) ->
  ProvidedHash = create(Password),
  case ProvidedHash == ExistingHash of
    true -> success;
    false -> fail
  end.

oauthCreate(Claims, Exp) ->
  Expiration = calendar:datetime_to_gregorian_seconds(calendar:universal_time()) + Exp,
  ewt:token(Expiration, Claims, ?ewtKey, sha256).

msgEncryption(Msg, PKey) ->
  % TODO: Change encryption to PGP Get around 500 character limit
  % Same Code just Test with PGP
  % LenPayload = length(erlang:binary_to_list(Msg)),
  %if
  %  LenPayload > 500 ->
  %    "Publisher Payload is to long to support Encryption (Decrease Publisher Payload to Under 500 Chars to support Encryption)";
  %  true ->
  EE = iolist_to_binary(PKey),
  [Entry] = public_key:pem_decode(EE),
  NewKey = public_key:pem_entry_decode(Entry),
  CMsg = public_key:encrypt_public(Msg, NewKey),
  base64:encode_to_string(CMsg).
  %end.

generatePass(Value) ->
  ShortName = lists:nth(1, string:tokens(string:to_lower(Value), ".")),
  lists:sublist(binary:bin_to_list(base64:encode(ShortName ++ ShortName ++ ShortName)), 3, 10).

certRenew() ->
  os:cmd("which acme-client && acme-client -v $(hostname).msgmap.io && nginx -s reload").

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
