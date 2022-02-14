%%%-------------------------------------------------------------------
%%% @author Ben Adams - Ben@MessageMap.IO
%%% @copyright (C) 2017-2022, MessageMap LLC
%%% @doc
%%%  Return to the user contents of their token
%%% @end
%%% Created : 8. Sept 2017
%%%-------------------------------------------------------------------
-module(user_handler).

-export([init/2]).

init(Req, Opts) ->
    {Claims, _} = tools:verifyAuth(Req),
    IsAdmin = tools:requireAdmin(erlang:is_map(Claims), Claims),
    Method = cowboy_req:method(Req),
    response(IsAdmin, Method, Req, Opts).

% Internal functions
response(<<"Bad">>, _, Req, Opts) ->
    ReqFinal = cowboy_req:reply(
        401,
        tools:resp_headers(),
        jiffy:encode(#{result => <<"Invalid Authorization">>}),
        Req
    ),
    {ok, ReqFinal, Opts};
response(_, Method, Req, Opts) ->
    Result = processRequest(Method, Req),
    Final = cowboy_req:reply(
        200,
        tools:resp_headers(),
        Result,
        Req
    ),
    {ok, Final, Opts}.

% Internal functions
processRequest(<<"GET">>, _) ->
    Result = buildResponses(database:getAllUsers()),
    jiffy:encode(Result);
processRequest(<<"POST">>, Req) ->
    {ok, Body, _} = cowboy_req:read_urlencoded_body(Req),
    {_, Username} = lists:keyfind(<<"username">>, 1, Body),
    {_, Password} = lists:keyfind(<<"password">>, 1, Body),
    database:saveDB(
        binary:bin_to_list(Username), binary:bin_to_list(Username), [], binary:bin_to_list(Password)
    ),
    Result = buildResponse(binary:bin_to_list(Username)),
    jiffy:encode(#{
        username => Result
    }).

%% Internal functions
buildResponses(Data) ->
    lists:map(
        fun(N) ->
            buildResponse(N)
        end,
        Data
    ).

buildResponse(Data) ->
    [Username, Permissions, Email, CreatedOn] = Data,
    #{
        username => binary:list_to_bin(Username),
        email => binary:list_to_bin(Email),
        permissions => binary:list_to_bin(Permissions),
        createdOn => binary:list_to_bin(tools:convertDateTime(CreatedOn))
    }.
