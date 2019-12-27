%%%-------------------------------------------------------------------
%%% @author Benjamin Adams
%%% @copyright (C) 2017, MessageMap.io
%%% @doc
%%%  This Intercepts Requests for Messages to DB Interaction
%%% @end
%%% Created : 16. Aug 2017
%%%-------------------------------------------------------------------
-module(messages).

-export([push/4]).
%-export([pull/4]).
-export([pull/2]).
-export([stats/2]).

-define(MAX_WAITING, 20000).

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
          % Get List of All Applications subscribed to Topic
          { SchemaId, Schema } = validate:lookup(Auth, TopicId, Version),
          MapPayload = jiffy:decode(Payload),
          if
            Schema == false ->
              process_Messages(TopicId, MapPayload, AppId, SchemaId),
              #{
                status => 'good'
              };
            true ->
              SchemaIsValid = validate:schema(Schema, MapPayload),
              if
                SchemaIsValid /= error ->
                  process_Messages(TopicId, MapPayload, AppId, SchemaId),
                  #{
                    status => 'good'
                  };
                true ->
                  #{
                    status => 'JSON Schema Validation'
                  }
                end
          end;
        true ->
          #{
            status => 'Application Doesn\'t Own the Topic'
          }
    end;
    true ->
      #{
        status => 'Application Doesn\'t Own the Topic'
      }
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
  [{_,_,_,_,_,Ownedtopics,SubscribedTopics,_,_}] = App,
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

process_Messages(TopicId, MapPayload, AppId, SchemaId) ->
  Apps = database:getAllAppDB(),
  lists:foreach(fun(App) ->
    %TODO: Add Push functions here (metrics, Notification, Websockets
    {_,FoundAppId,_,_,_,_,SubscribedTopics,_,EncryptValue} = App,
    case lists:member(hd(TopicId), SubscribedTopics) of
      true ->
        SavedPayload = case EncryptValue of
           [] ->
             MapPayload;
           _ ->
             binary:list_to_bin(encryption:msgEncryption(jiffy:encode(MapPayload), EncryptValue))
         end,
        %Insert data by App For Pulling
        Tbl = database:check_dyn_table(FoundAppId),
        % TODO: Check Table row count limit 20,000
        Waiting = mnesia:table_info(Tbl, size),
        tools:log("info", io_lib:format("Current (~p) Messages: ~p", [Tbl, Waiting])),
        if
          Waiting < ?MAX_WAITING ->
            database:insert_dyn_table(Tbl, AppId, TopicId, SchemaId, SavedPayload);
          true -> true
        end;
      _ ->
        true
    end
  end, Apps),
  database:add_published_counter(AppId).
