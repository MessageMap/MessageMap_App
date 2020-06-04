%%%-------------------------------------------------------------------
%%% @author Benjamin Adams
%%% @copyright (C) 2020, MessageMap.io
%%% @doc
%%%  This Intercepts Requests for Messages to DB Interaction
%%% @end
%%% Created : 29. May 2020
%%%-------------------------------------------------------------------
-module(messages).

-export([push/4]).
-export([pull/2]).
-export([stats/2]).

-define(MAX_WAITING, 20000).

% Don't allow latest yet
push("latest", _, _ , _) ->
  { 422, #{
    status => 'Topic Is attached to Schemas Send message with schema: /messages/{SchemaVersion}/{topicName}'
  } };
push(Version, TopicName, Auth, Payload) ->
  { _, Scope } = maps:find(scope, Auth),
  Check = check_scope("PUSH", Scope),
  if
    Check ->
      { _, TopicId } = pullIds(Auth, TopicName, ownedTopics),
      { ok , AppId } = maps:find(id, Auth),
      % Add Schemas (For Validation)
      if
        TopicId /= fail ->
          { SchemaId, Schema } = validate:lookup(Auth, TopicId, Version),
          MapPayload = jiffy:decode(Payload),
          if
            Schema == false ->
              % Update to Say Queue status Max waiting or message size if Encryption enabled
              process_Messages(TopicId, MapPayload, AppId, SchemaId);
            Schema == "Schema Version Not Found" ->
              { 422, #{
                status => 'Invalid Schema Name'
              } };
            true ->
              SchemaIsValid = validate:schema(Schema, MapPayload),
              if
                SchemaIsValid /= error ->
                  process_Messages(TopicId, MapPayload, AppId, SchemaId);
                true ->
                  { 422, #{
                    status => 'JSON Schema Validation'
                  } }
                end
          end;
        true ->
          { 422, #{
            status => 'Application Doesn\'t Own the Topic'
          } }
    end;
    true ->
      {422, #{
        status => 'Application Doesn\'t Own the Topic'
      } }
  end.

pull(Auth, Limit) ->
  AppId = pullAppId(Auth),
  Tbl = database:check_dyn_table(AppId),
  if
    Limit < 100 ->
      database:get_dyn_table(Tbl, Limit);
    true ->
      database:get_dyn_table(Tbl, 100)
  end.

stats(TopicName, Auth) ->
  { _, Scope } = maps:find(scope, Auth),
  Check =  check_scope("PULL", Scope),
  if
    Check ->
      messages:status(TopicName);
    true ->
      #{
        status => 'Topic Not Found'
      }
  end.

%%%%% Internal Functions
% all the Variations
check_scope("PULL", [<<"read">>,_]) ->
  true;
check_scope("PULL", [_,<<"read">>]) ->
  true;
check_scope("PULL", [<<"read">>]) ->
  true;
check_scope("PUSH", [<<"write">>,_]) ->
  true;
check_scope("PUSH", [_,<<"write">>]) ->
  true;
check_scope("PUSH", [<<"write">>]) ->
  true;
check_scope(_, _) ->
  false.

% Pull Basic Variables
pullIds(Auth, TopicName, Method) ->
    { ok , AppId } = maps:find(id, Auth),
    App = database:getAppDBAppId(AppId),
    Topic = database:getTopicDB(binary:bin_to_list(TopicName)),
    if
      Topic == [] ->
        {[],fail};
      true ->
        [{_,TopicId,_,_,_,_}] = Topic,
        ValidTopicId = check_topicId_in_App(TopicId, App, Method),
        {App, ValidTopicId}
    end.

pullAppId(Auth) ->
    { ok, AppId } = maps:find(id, Auth),
    AppId.

check_topicId_in_App(TopicId, App, Method) ->
  [{_,_,_,_,_,Ownedtopics,SubscribedTopics,_,_,_}] = App,
  ArrayOfIds = if
    Method == topicsSubscribed ->
      SubscribedTopics;
    Method == ownedTopics ->
      Ownedtopics;
    true ->
      ""
  end,
  case lists:member(hd(TopicId), ArrayOfIds) of
    true ->
      TopicId;
    _ ->
      fail
  end.

processMessageMap(TopicId, Filter, Payload) ->
    {[ {_, Tid}, {_,Values} ]}  = Filter,
    LTid = binary:bin_to_list(Tid),
    if
      TopicId =:= LTid ->
        mapping:msgMapper(Values, Payload);
      true ->
        Payload
    end.

process_Messages(TopicId, MapPayload, AppId, SchemaId) ->
  Apps = database:getAllAppDB(),
  R = lists:map(fun(App) ->
    %TODO: Add Push functions here (metrics, Notification, Websockets
    {_,FoundAppId,_,_,_,_,SubscribedTopics,_,FilterSettings, EncryptValue} = App,
    CaseResult = case string:str(SubscribedTopics, TopicId) of
      0 ->
        "bad";
      _ ->
        % Start MessageMapping
        MapMessageResult = case FilterSettings of
           <<"[]">> ->
             MapPayload;
           _ ->
             JsonFilter = [ processMessageMap(TopicId, X, MapPayload) || X <- jiffy:decode(FilterSettings) ],
             lists:nth(1, JsonFilter)
        end,
        % Start Encryption Values
        SavedPayload = case EncryptValue of
           [] ->
             MapMessageResult;
           _ ->
             binary:list_to_bin(encryption:msgEncryption(jiffy:encode(MapMessageResult), binary:list_to_bin(EncryptValue)))
        end,
        Tbl = database:check_dyn_table(FoundAppId),
        % TODO: Check Table row count limit 20,000
        Waiting = mnesia:table_info(Tbl, size),
        tools:log("info", io_lib:format("Current (~p) Messages: ~p", [Tbl, Waiting])),
        Result = if
          Waiting < ?MAX_WAITING andalso SavedPayload =/= "Payload Is To Long" ->
            database:insert_dyn_table(Tbl, AppId, TopicId, SchemaId, SavedPayload),
            tools:log("info", io_lib:format("Message Published", [])),
            true;
          true ->
            tools:log("info", io_lib:format("Table: ~p Queue has payload to long or Queue is Maxed: ~p", [Tbl, Waiting])),
            false
        end,
        Result
    end,
    CaseResult
  end, Apps),
  database:add_published_counter(AppId),
  Success = length(lists:filter(fun(X) -> X =:= true end, R)),
  Failed = length(lists:filter(fun(X) -> X =:= false end, R)),
  if
    0 =:= Failed ->
      { 200, #{
          status => 'mix',
          subscrier_Result => #{
               success => Success,
               failed => Failed
          }
        }
      };
    true ->
      { 207, #{
          status => 'good',
          subscriber_Results => #{
               success => Success,
               failed => Failed
          }
        }
      }
  end.
