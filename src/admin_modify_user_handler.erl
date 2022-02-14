%%%-------------------------------------------------------------------
%%% @author Ben Adams - Ben@MessageMap.IO
%%% @copyright (C) 2017-2022, MessageMap LLC
%%% @doc
%%%  Administration Modify User to system
%%% @end
%%% Created : 3. Sept 2019
%%%-------------------------------------------------------------------
-module(admin_modify_user_handler).

-export([init/2]).

init(Req, Opts) ->
    {Claims, _} = tools:verifyAuth(Req),
    IsAdmin = tools:requireAdmin(erlang:is_map(Claims), Claims),
    Method = cowboy_req:method(Req),
    response(IsAdmin, Method, Req, Opts).

% Internal functions
response(<<"Bad">>, _, Req, Opts) ->
    ReqFinal = cowboy_req:reply(
        200,
        tools:resp_headers(),
        jiffy:encode(#{result => <<"Invalid Authorization">>}),
        Req
    ),
    {ok, ReqFinal, Opts};
response(_, Method, Req, Opts) ->
    Result = processRequest(Method, Req),
    ReqFinal = cowboy_req:reply(
        200,
        tools:resp_headers(),
        Result,
        Req
    ),
    {ok, ReqFinal, Opts}.

processRequest(<<"POST">>, Req) ->
    {ok, Body, _} = cowboy_req:read_urlencoded_body(Req),
    {_, Username} = lists:keyfind(<<"email">>, 1, Body),
    {_, Permissions} = lists:keyfind(<<"permissions">>, 1, Body),
    {_, Password} = lists:keyfind(<<"password">>, 1, Body),
    if
        Username =:= 1 ->
            jiffy:encode(#{"Error" => "Bad Username"});
        Password =:= 1 ->
            jiffy:encode(#{"Error" => "Bad Password"});
        true ->
            database:deleteDBUser(binary_to_list(Username)),
            database:storeDB(
                binary_to_list(Username),
                binary_to_list(Username),
                [binary_to_list(Permissions)],
                binary_to_list(Password)
            ),
            jiffy:encode(#{update => true})
    end.
