%%%-------------------------------------------------------------------
%%% @author Benjamin Adams
%%% @copyright (C) 2020, MessageMap.io
%%% @doc
%%%  Used to validate post payload with version
%%% @end
%%% Created : 1/4/2020 12:51
%%%-------------------------------------------------------------------
-module(validate).

-export([lookup/3, schema/2]).

lookup(_, _, "None") ->
  { false, false };
lookup(Auth, TopicId, Version) ->
  { ok , Info } = maps:find(info, Auth),
  { ok, AuthSchemas } = maps:find(schemas, Info),
  filteredSchemas(AuthSchemas, Auth, TopicId, Version).

filteredSchemas([], _, _, _) ->
  { false, "Schema Version Not Found" };
filteredSchemas(_,_,_,"latest") ->
  %TODO: Remove this for when we have latest added Next Version
  { false, "Schema Version Not Found" };
filteredSchemas(AuthSchemas, _, _, Version) ->
  AuthV = lists:filter(fun(X) -> maps:get(v, X) == binary:bin_to_list(Version) end, AuthSchemas),
  pullSchemaList(AuthV).

pullSchemaList([]) ->
  { false, "Schema Version Not Found" };
pullSchemaList(ListAuth) ->
  [AuthV] = ListAuth,
  {ok, Sid} = maps:find(id, AuthV),
  {Sid, pullSchema(database:getSchemaDBSchemaId(Sid)) }.

pullSchema([{_,_,_,Schema, _}]) ->
  Schema;
pullSchema(_) ->
  "Schema Not Found".

schema(Schema, Payload) ->
  validateSchema(catch jiffy:decode(Schema), Payload).

validateSchema({ error, _}, _) ->
  error;
validateSchema(S, Payload) ->
  {Result, _} = jesse:validate_with_schema(S, Payload),
  tools:log("info", io_lib:format("Result: ~p", [Result])),% jesse:validate_with_schema(S, Payload)])),
  Result.
