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
-export([pull/4]).
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
              Apps = database:getAllAppDB(),
              lists:foreach(fun(App) ->
                %TODO: Add Push functions here (metrics, Notification, Websockets
                {_,FoundAppId,_,_,_,_,SubscribedTopics,_} = App,
                case lists:member(hd(TopicId), SubscribedTopics) of
                  true ->
                    %Insert data by App For Pulling
                    Tbl = database:check_dyn_table(FoundAppId),
                    % TODO: Check Table row count limit 20,000
                    Waiting = mnesia:table_info(Tbl, size),
                    tools:log("info", io_lib:format("Current (~p) Messages: ~p", [Tbl, Waiting])),
                    if
                       Waiting < ?MAX_WAITING ->
                         database:insert_dyn_table(Tbl, AppId, TopicId, SchemaId, MapPayload);
                       true -> true
                    end;
                  _ ->
                    true
                end
              end, Apps),
              #{
                status => 'good'
              };
            true ->
              SchemaIsValid = validate:schema(Schema, MapPayload),
              if
                SchemaIsValid /= error ->
                  Apps = database:getAllAppDB(),
                  lists:foreach(fun(App) ->
                    %TODO: Add Push functions here (metrics, Notification, Websockets
                    {_,FoundAppId,_,_,_,_,SubscribedTopics,_} = App,
                    case lists:member(hd(TopicId), SubscribedTopics) of
                      true ->
                        %Insert data by App For Pulling
                        Tbl = database:check_dyn_table(FoundAppId),
                        % TODO: Check Table Row count limit 20,000
                        Waiting = mnesia:table_info(Tbl, size),
                        tools:log("info", io_lib:format("Current (~p) Messages: ~p", [Tbl, Waiting])),
                        if
                          Waiting < ?MAX_WAITING ->
                            database:insert_dyn_table(Tbl, AppId, TopicId, SchemaId, MapPayload);
                          true -> true
                        end;
                      _ ->
                        true
                    end
                  end, Apps),
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
            status => 'Insert Error'
          }
    end;
    true ->
      #{
        status => 'Insert Error'
      }
  end.

pull(Version, TopicName, Auth, Limit) ->
  %io:format("Pulling Messages Count: ~p", [Limit]),
  { _, Scope } = maps:find(scope, Auth),
  Check =  check_scope("PULL", Scope),
  if
    Check ->
      {App, TopicId} = pullIds(Auth, TopicName, topicsSubscribed),
      if
        TopicId /= fail ->
          [{_,AppId,_,_,_,_,_,_}] = App,
          Tbl = database:check_dyn_table(AppId),
          database:get_dyn_table(Tbl, Limit);
        true ->
          #{
            status => 'Post Message Error'
          }
    end;
    true ->
      #{
        status => 'Application Not Subscribed to topic'
      }
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

check_topicId_in_App(TopicId, App, Method) ->
  [{_,_,_,_,_,Ownedtopics,SubscribedTopics,_}] = App,
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