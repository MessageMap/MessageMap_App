%%%-------------------------------------------------------------------
%%% @author Ben Adams - Ben@MessageMap.IO
%%% @copyright (C) 2017-2022, MessageMap LLC
%%% @doc
%%% Module will be used for encryption
%%% @end
%%% Created : 1. Sept 2017
%%%-------------------------------------------------------------------
-module(encryption).
-include_lib("public_key/include/public_key.hrl").

-export([create/1]).
-export([ewtCreate/3]).
-export([ewtDecode/1]).
-export([generatePass/1]).
-export([oauthCreate/1]).
-export([validate/2]).
-export([msgEncryption/2]).

-define(saltround, 10).
-define(ewtKey, tools:configFile("auth_key")).
-define(ewtTimeout, tools:configFile("auth_timeout")).

create(Password) ->
    mcrypto:encrypt(Password, ?ewtKey).

ewtCreate(Name, Email, Permissions) ->
    Expiration =
        calendar:datetime_to_gregorian_seconds(calendar:universal_time()) +
            1 * list_to_integer(?ewtTimeout),
    Claims = #{
        <<"iss">> => <<"MessageMap">>,
        <<"exp">> => Expiration,
        user => Email,
        name => Name,
        roles => [Permissions]
    },
    [JWK, JWS] = setupJWTRequirements(),
    Signed = jose_jwt:sign(JWK, JWS, Claims),
    {_, Value} = jose_jws:compact(Signed),
    Value.

ewtDecode(Ewt) ->
    [JWK, _] = setupJWTRequirements(),
    {_, {jose_jwt, Value}, _} = jose_jwt:verify(JWK, Ewt),
    Value.

validate(Password, ExistingHash) ->
    mcrypto:validation(Password, ExistingHash).

oauthCreate(Claims) ->
    [JWK, JWS] = setupJWTRequirements(),
    Signed = jose_jwt:sign(JWK, JWS, Claims),
    {_, Value} = jose_jws:compact(Signed),
    Value.

msgEncryption(Msg, PKey) ->
    LenPayload = length(erlang:binary_to_list(Msg)),
    if
        LenPayload > 500 ->
            "Publisher Payload is to long to support Encryption (Decrease Publisher Payload to Under 500 Chars to support Encryption)";
        % Fix for this is for Enterprise level using our client and key to deycrypt
        % on encrypt string split full string by 400 Chars and in encrypt then combind to one string
        true ->
            EE = iolist_to_binary(PKey),
            [Entry] = public_key:pem_decode(EE),
            NewKey = public_key:pem_entry_decode(Entry),
            CMsg = public_key:encrypt_public(Msg, NewKey),
            base64:encode_to_string(CMsg)
    end.

generatePass(Value) ->
    ShortName = lists:nth(1, string:tokens(string:to_lower(Value), ".")),
    lists:sublist(binary:bin_to_list(base64:encode(ShortName ++ ShortName ++ ShortName)), 3, 10).

%%%% Internal Functions
setupJWTRequirements() ->
    JWK = #{
        <<"kty">> => <<"oct">>,
        <<"k">> => jose_base64url:encode(?ewtKey)
    },
    JWS = #{
        <<"alg">> => <<"HS256">>
    },
    [JWK, JWS].
