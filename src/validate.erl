%%%-------------------------------------------------------------------
%%% @author Benjamin Adams
%%% @copyright (C) 2017, MessageMap.io
%%% @doc
%%%  Used to validate post payload with version
%%% @end
%%% Created : 12/14/2017
%%%-------------------------------------------------------------------
-module(validate).

-export([lookup/3, schema/2]).

lookup(Auth, TopicId, Version) ->
  { ok , Info } = maps:find(info, Auth),
  % TODO: test Multi topic with topic Id matching attached schema
  { ok, AuthSchemas } = maps:find(schemas, Info),
  filteredSchemas(AuthSchemas, Auth, TopicId, Version).

filteredSchemas([], _, _, _) ->
  { false, false };
filteredSchemas(AuthSchemas, Auth, TopicId, Version) ->
  tools:log("info", io_lib:format("AuthSchema: ~p - Auth: ~p - TopicID: ~p Version: ~p", [AuthSchemas, Auth, TopicId, Version])),
  [AuthV] = lists:filter(fun(S) ->
    { ok, AuthVersion } = maps:find(v, S),
    Version == list_to_binary(AuthVersion)
  end, AuthSchemas),
  {ok, Sid} = maps:find(id, AuthV),
  [{_, _, _, Schema, _}] = database:getSchemaDBSchemaId(Sid),
  {Sid, Schema}.

schema(Schema, Payload) ->
  S = jiffy:decode(Schema),
  {Result, _} = jesse:validate_with_schema(S, Payload),
  %tools:log("info", io_lib:format("Result: ~p", [jesse:validate_with_schema(S, Payload)])),
  Result.
