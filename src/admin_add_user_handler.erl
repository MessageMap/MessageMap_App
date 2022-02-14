%%%-------------------------------------------------------------------
%%% @author Ben Adams - Ben@MessageMap.IO
%%% @copyright (C) 2017-2022, MessageMap LLC
%%% @doc
%%%  Administration Add User to system
%%% @end
%%% Created : 8. Sept 2017
%%%-------------------------------------------------------------------
-module(admin_add_user_handler).

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

% Internal functions
processRequest(<<"POST">>, Req) ->
    {ok, Body, _} = cowboy_req:read_urlencoded_body(Req),
    {_, Username} = lists:keyfind(<<"email">>, 1, Body),
    {_, Permissions} = lists:keyfind(<<"permissions">>, 1, Body),
    {_, Password} = lists:keyfind(<<"password">>, 1, Body),
    tools:log("info", io_lib:format("New User: ~p~n", [Username])),
    if
        Username =:= 1 ->
            jiffy:encode(#{"Error" => "Bad Username"});
        Password =:= 1 ->
            jiffy:encode(#{"Error" => "Bad Password"});
        true ->
            database:storeDB(
                binary_to_list(Username),
                binary_to_list(Username),
                [binary_to_list(Permissions)],
                binary_to_list(Password)
            ),
            jiffy:encode(#{update => true})
    end.
