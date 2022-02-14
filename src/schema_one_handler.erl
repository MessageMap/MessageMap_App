%%%-------------------------------------------------------------------
%%% @author Ben Adams - Ben@MessageMap.IO
%%% @copyright (C) 2017-2022, MessageMap LLC
%%% @doc
%%%  Return One topic for listing
%%% @end
%%% Created : 8. Sept 2017
%%%-------------------------------------------------------------------
-module(schema_one_handler).

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
    Id = cowboy_req:binding(schemaId, Req),
    Result = processRequest(Method, Claims, Id, Req),
    {ok, ReqFinal} = cowboy_req:reply(
        200,
        tools:resp_headers(),
        Result,
        Req
    ),
    {ok, ReqFinal, Opts}.

% Internal functions
processRequest(<<"GET">>, _, SchemaId, _) ->
    SchemaData = database:getSchemaDBSchemaId(binary:bin_to_list(SchemaId)),
    Result = buildResponse(element(1, list_to_tuple(SchemaData))),
    jiffy:encode(Result);
processRequest(<<"DELETE">>, _, SchemaId, _) ->
    %%TODO: This is not removing the schema from the DB
    database:deleteSchemaDBSchemaId(binary:bin_to_list(SchemaId)),
    jiffy:encode([]).

% %% Internal functions
buildResponse(Data) ->
    {_, Id, Version, Validation, CreatedOn} = element(1, list_to_tuple([Data])),
    #{
        id => binary:list_to_bin(Id),
        validation => binary:list_to_bin(Validation),
        version => binary:list_to_bin(Version),
        createdOn => binary:list_to_bin(tools:convertDateTime(CreatedOn))
    }.
