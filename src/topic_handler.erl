%%%-------------------------------------------------------------------
%%% @author Ben Adams - Ben@MessageMap.IO
%%% @copyright (C) 2017-2022, MessageMap LLC
%%% @doc
%%%  Return to the user contents of their token
%%% @end
%%% Created : 8. Sept 2017
%%%-------------------------------------------------------------------
-module(topic_handler).

-export([init/2]).

init(Req, Opts) ->
    {Claims, Req2} = tools:verifyAuth(Req),
    Method = cowboy_req:method(Req),
    response(Claims, Method, Req2, Opts).

% Internal functions
response(<<"Bad">>, Method, Req, Opts) ->
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
    {_, TopicName} = lists:keyfind(<<"topicName">>, 1, Body),
    if
        TopicName =:= 1 ->
            jiffy:encode(#{"Error" => "Bad Topic Name or Missing Topic name"});
        true ->
            TopicData = database:saveTopic(binary:bin_to_list(TopicName), "", ""),
            Result = buildResponse(element(1, list_to_tuple(TopicData))),
            jiffy:encode(Result)
    end;
processRequest(<<"GET">>, _, _) ->
    AllTopics = database:getAllTopicDB(),
    Results = buildResponses(AllTopics),
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
    {_, Id, Name, Description, SchemaName, CreatedOn} = element(1, list_to_tuple([Data])),
    #{
        id => binary:list_to_bin(Id),
        name => binary:list_to_bin(Name),
        description => binary:list_to_bin(Description),
        schemaName => binary:list_to_bin(SchemaName),
        createdOn => binary:list_to_bin(tools:convertDateTime(CreatedOn))
    }.
