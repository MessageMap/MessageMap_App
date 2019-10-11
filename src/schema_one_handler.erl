%%%-------------------------------------------------------------------
%%% @author Benjamin Adams
%%% @copyright (C) 2017, Message.io
%%% @doc
%%%  Return One topic for listing
%%% @end
%%% Created : 8. Sept 2017
%%%-------------------------------------------------------------------
-module(schema_one_handler).

-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {}).

init(_, Req, _Opts) ->
  {ok, Req, #state{}}.

handle(Req, State=#state{}) ->
  { Claims, Req2 } = tools:verifyAuth(Req),
  { Method, _ } = cowboy_req:method(Req),
  { Id, _} = cowboy_req:binding(schemaId, Req),
  Result = processRequest(Method, Claims, Id, Req),
  { ok, ReqFinal } = cowboy_req:reply(200, tools:resp_headers(),
      Result,
      Req2),
  {ok, ReqFinal, State}.

terminate(_Reason, _Req, _State) ->
  ok.

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
  {_,Id,Version,Validation,CreatedOn} = element(1, list_to_tuple([Data])),
  #{
    id => binary:list_to_bin(Id),
    validation => binary:list_to_bin(Validation),
    version => binary:list_to_bin(Version),
    createdOn => binary:list_to_bin(tools:convertDateTime(CreatedOn))}.
