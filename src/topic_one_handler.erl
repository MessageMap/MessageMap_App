%%%-------------------------------------------------------------------
%%% @author Ben Adams - Ben@MessageMap.IO
%%% @copyright (C) 2017-2022, MessageMap LLC
%%% @doc
%%%  Return One topic for listing
%%% @end
%%% Created : 04. Jun 2020
%%%-------------------------------------------------------------------
-module(topic_one_handler).

-export([init/2]).

init(Req, Opts) ->
    {Claims, Req2} = tools:verifyAuth(Req),
    Method = cowboy_req:method(Req),
    response(Claims, Method, Req2, Opts).

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
    Id = cowboy_req:binding(topicId, Req),
    Result = processRequest(Method, Claims, Id, Req),
    ReqFinal = cowboy_req:reply(
        200,
        tools:resp_headers(),
        Result,
        Req
    ),
    {ok, ReqFinal, Opts}.

processRequest(<<"GET">>, _, TopicId, _) ->
    TopicData = database:getTopicDBTopicId(binary:bin_to_list(TopicId)),
    Result = buildResponse(element(1, list_to_tuple(TopicData))),
    jiffy:encode(Result);
processRequest(<<"PUT">>, _, TopicId, Req) ->
    {ok, Body, _} = cowboy_req:read_urlencoded_body(Req),
    {_, Name} = lists:keyfind(<<"name">>, 1, Body),
    {_, Description} = lists:keyfind(<<"description">>, 1, Body),
    {_, SchemaId} = lists:keyfind(<<"schemaid">>, 1, Body),
    {_, TopicData} = database:updateTopicDBTopicId(
        binary:bin_to_list(TopicId), Name, Description, SchemaId
    ),
    Result = buildResponse(element(1, list_to_tuple(TopicData))),
    jiffy:encode(Result);
processRequest(<<"DELETE">>, _, TopicId, _) ->
    database:deleteTopicDBTopicId(binary:bin_to_list(TopicId)),
    jiffy:encode([]).

% %% Internal functions
buildResponse(Data) ->
    {_, Id, Name, Description, SchemaId, CreatedOn} = element(1, list_to_tuple([Data])),
    #{
        id => binary:list_to_bin(Id),
        name => binary:list_to_bin(Name),
        description => binary:list_to_bin(Description),
        schemaId => binary:list_to_bin(SchemaId),
        createdOn => binary:list_to_bin(tools:convertDateTime(CreatedOn))
    }.
