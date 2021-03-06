%%%-------------------------------------------------------------------
%%% @author Ben Adams - Ben@MessageMap.IO
%%% @copyright (C) 2017-2022, MessageMap LLC
%%% @doc
%%%  Return the list of applications that have been created
%%% @end
%%% Created : 4. Jun 2020
%%%-------------------------------------------------------------------
-module(application_handler).

-export([init/2]).

init(Req, Opts) ->
    {Claims, _} = tools:verifyAuth(Req),
    Method = cowboy_req:method(Req),
    response(Claims, Method, Req, Opts).

% Internal functions
response(<<"Bad">>, _, Req, Opts) ->
    ReqFinal = cowboy_req:reply(
        200,
        tools:resp_headers(),
        jiffy:encode(#{result => <<"Invalid Authorization">>}),
        Req
    ),
    {ok, ReqFinal, Opts};
response(Claims, Method, Req, Opts) ->
    Result = processRequest(Method, Claims, Req),
    ReqFinal = cowboy_req:reply(
        200,
        tools:resp_headers(),
        Result,
        Req
    ),
    {ok, ReqFinal, Opts}.

processRequest(<<"POST">>, _, Req) ->
    {ok, Body, _} = cowboy_req:read_urlencoded_body(Req),
    {_, AppName} = lists:keyfind(<<"appName">>, 1, Body),
    if
        AppName =:= 1 ->
            jiffy:encode(#{<<"Error">> => <<"Bad Application Name or Missing Application name">>});
        true ->
            AppData = database:saveApp(
                binary:bin_to_list(AppName), "", [], [], [], [], [], [], [], [], []
            ),
            Result = buildResponse(element(1, list_to_tuple(AppData))),
            jiffy:encode(Result)
    end;
processRequest(<<"GET">>, _, _) ->
    AllApps = database:getAllAppDB(),
    Results = buildResponses(AllApps),
    jiffy:encode(Results).

%% Internal functions
buildResponses(Data) ->
    lists:map(
        fun(N) ->
            buildResponse(N)
        end,
        Data
    ).

buildResponse(Data) ->
    {_, Id, Name, Description, ApiKeys, Ownedtopics, SubscribedTopics, CreatedOn, Filter, Encrypt,
        PushMessages, PushUrl, PushRetries, PushStatusCode, PushHeaders} = element(
        1, list_to_tuple([Data])
    ),
    #{
        id => binary:list_to_bin(Id),
        name => binary:list_to_bin(Name),
        description => binary:list_to_bin(Description),
        apiKeys => binary:list_to_bin(ApiKeys),
        ownedTopics => binary:list_to_bin(Ownedtopics),
        subscribedTopics => binary:list_to_bin(SubscribedTopics),
        encrypt => binary:list_to_bin(Encrypt),
        filters => binary:list_to_bin([Filter]),
        pushMessages => PushMessages,
        pushUrl => PushUrl,
        pushRetries => PushRetries,
        pushStatusCode => PushStatusCode,
        pushHeaders => PushHeaders,
        createdOn => binary:list_to_bin(tools:convertDateTime(CreatedOn))
    }.
