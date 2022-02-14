%%%-------------------------------------------------------------------
%%% @author Ben Adams - Ben@MessageMap.IO
%%% @copyright (C) 2017-2022, MessageMap LLC
%%% @doc
%%%  Return application status to the UI
%%% @end
%%% Created : 11, Jun 2020
%%%-------------------------------------------------------------------
-module(stats_handler).

-export([init/2]).

init(Req, Opts) ->
    {Claims, Req2} = tools:verifyAuth(Req),
    response(Claims, Req2, Opts).

% Internal functions
response(<<"Bad">>, Req, Opts) ->
    ReqFinal = cowboy_req:reply(
        200,
        tools:resp_headers(),
        jiffy:encode(#{result => <<"Invalid Authorization">>}),
        Req
    ),
    {ok, ReqFinal, Opts};
response(_, Req, Opts) ->
    AppId = cowboy_req:binding(appId, Req),
    Result = buildResponse(binary_to_list(AppId)),
    ReqFinal = cowboy_req:reply(
        200,
        tools:resp_headers(),
        jiffy:encode(Result),
        Req
    ),
    {ok, ReqFinal, Opts}.

% Internal functions
buildResponse(AppId) ->
    Tbl = database_manager:allTblNames(AppId),
    PubPull = mnesia:dirty_read({counter_published, AppId}),
    Pub = resultConversion(PubPull),
    SubPull = mnesia:dirty_read({counter_consumed, AppId}),
    Sub = resultConversion(SubPull),
    Size =
        if
            Tbl =:= [] ->
                0;
            true ->
                lists:foldl(fun(X, Sum) -> mnesia:table_info(X, size) + Sum end, 0, Tbl)
        end,
    #{
        id => AppId,
        messages_waiting => Size,
        published_messages => binary:list_to_bin(Pub),
        consumed_messages => binary:list_to_bin(Sub)
    }.

resultConversion([]) ->
    ["0"];
resultConversion([{_, _, Result}]) ->
    lists:flatten(io_lib:format("~p", [Result])).
