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
    %   io:format("~n~n====================================~nClaims: ~p~n JWK: ~p~nJWS: ~p~n", [Claims, JWK, JWS]),
    %   jose_jwt:sign(#{<<"k">> => <<"TXlFbmNyJlB5SU9O">>,<<"kty">> => <<"oct">>},
    %     #{<<"alg">> => <<"HS256">>}, #{app =>
    %               #{encrypt =>
    %                     "-----BEGIN PUBLIC KEY-----\nMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAvtAapGh/2B3tBVkhGmtt\nIPukGesnqVRcssl6JJS2zSPr7etJFLzH9lf6QyrC9VS4Wz+EyK9OHx9O5/hDkquK\nku1te870S/YcNIqiaCBLF0ddv6cvvj0DPmZQdIFDRlgjyk9jZrzMuemDXSyTyMET\non/Vuk/M9ZY6+yPMzeVGcffDzHyCg2zp4B0EqkrZWq3An6ePBGzEhrRq8IyeVgfA\ncQCBBkvzn6Fu7ufFxAUGiA2TStKA4TUgxaYTCbIy5hhumbMGqdaPzmwFrX1w01nQ\nOeXAWCPqOgqRss0R4u6itOENZ0YF5FlChEEj2N64qNdg6wQoI639hpxPcxQzhgCM\noQIDAQAB\n-----END PUBLIC KEY-----",
    %                 filters => <<"[]">>,
    %                 id => "79e3da79-ec28-44a0-a3db-9f33b80f9800",
    %                 name => "qespshwadetphnrscersijygo",ownedTopics => [],
    %                 pushHeaders => <<>>,pushMessages => <<"false">>,
    %                 pushRetries => <<>>,pushStatusCode => <<>>,pushUrl => <<>>,
    %                 subscribedTopics => "55c7814d-6d3b-4e39-a617-a01a45bc9dc4"},
    %           <<"exp">> => 63808458540,<<"iss">> => <<"MessageMap - App">>})
    %  JWK: #{<<"k">> => <<"TXlFbmNyJlB5SU9O">>,<<"kty">> => <<"oct">>}
    % JWS: #{<<"alg">> => <<"HS256">>}
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
