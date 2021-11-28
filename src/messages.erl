%%%-------------------------------------------------------------------
%%% @author Benjamin Adams
%%% @copyright (C) 2020, MessageMap.io
%%% @doc
%%%  This Intercepts Requests for Messages to DB Interaction
%%% @end
%%% Created : 29. May 2020
%%%-------------------------------------------------------------------
-module(messages).

-export([push/5]).
-export([pull/2]).
-export([stats/2]).

% Don't allow latest yet
push("latest", _, _ , _, _) ->
  { 422, #{
    status => 'Topic Is attached to Schemas Send message with schema: /messages/{SchemaVersion}/{topicName}'
  } };
push(Version, TopicName, Auth, Payload, RequestTime) ->
  { _, Scope } = maps:find(<<"scope">>, Auth),
  Check = check_scope("PUSH", Scope),
  if
    Check ->
      { _, TopicId } = pullIds(Auth, TopicName, ownedTopics),
      { ok , AppId } = maps:find(<<"id">>, Auth),
      % Add Schemas (For Validation)
      if
        TopicId /= fail ->
          { SchemaId, Schema } = validate:lookup(Auth, TopicId, Version),
          MapPayload = try
               jiffy:decode(Payload)
          catch
               _Type:_Error ->
                   "Invalid"
          end,
          if
            MapPayload =:= "Invalid" ->
              { 422, #{
                 status => 'Invalid JSON Schema'
              } };
            Schema == false ->
              % Update to Say Queue status Max waiting or message size if Encryption enabled
              process_Messages(TopicId, MapPayload, AppId, SchemaId, RequestTime);
            Schema == "Schema Version Not Found" ->
              { 422, #{
                status => 'Invalid Schema Name'
              } };
            true ->
              SchemaIsValid = validate:schema(Schema, MapPayload),
              if
                SchemaIsValid /= error ->
                  process_Messages(TopicId, MapPayload, AppId, SchemaId, RequestTime);
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
  { Tbl, RowCount } = database_manager:selectTblName(AppId),
  { Result, RollTable } = if
    Tbl =:= false ->
      { [], true };
    Limit < 100 ->
      { database:get_dyn_table(AppId, Tbl, Limit), (RowCount < Limit) };
    true ->
      { database:get_dyn_table(AppId, Tbl, 100), (RowCount < 100) }
  end,
  rollTable( Result, RollTable, Tbl, Auth, Limit, AppId ).

stats(TopicName, Auth) ->
  { _, Scope } = maps:find(<<"scope">>, Auth),
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
rollTable(Result, false, _, _, _, _) ->
   Result;
rollTable(Result, _, false, _, _, _ ) ->
   Result;
rollTable(Result, true, Tbl, Auth, Limit, AppId) ->
   database_manager:deleteTblName(Tbl, AppId),
   lists:merge(Result, pull(Auth, Limit)).

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
    { ok , AppId } = maps:find(<<"id">>, Auth),
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
    { ok, AppId } = maps:find(<<"id">>, Auth),
    AppId.

check_topicId_in_App(TopicId, App, Method) ->
  [{_,_,_,_,_,Ownedtopics,SubscribedTopics,_,_,_,_,_,_,_,_}] = App,
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

process_Messages(TopicId, MapPayload, AppId, SchemaId, RequestTime) ->
  Apps = database:getAllAppDB(),
  R = lists:map(fun(App) ->
    {_,FoundAppId,_,_,_,_,SubscribedTopics,_,FilterSettings, EncryptValue, PushMessages, PushUrl, PushRetries, PushStatusCode, PushHeaders } = App,
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
        % TODO: this is where Push and not Save Action will happen
        % This is where Push will happen and return Fail Max for save in next section
        PushResult = if
            PushMessages =:= <<"true">> andalso SavedPayload =/= "Payload Is To Long" ->
               Headers = [ erlang:list_to_tuple(lists:map(fun(X) -> lists:delete($;, X) end, string:tokens(H, ":")))
                           || H <- string:tokens(erlang:binary_to_list(PushHeaders), "\n")],
               pushMessages(erlang:binary_to_list(PushUrl), erlang:binary_to_integer(PushStatusCode), Headers, jiffy:encode(SavedPayload), erlang:binary_to_integer(PushRetries));
            true ->
               true
        end,
        HardDriveNotFull = true, % Change This to if HD is Full
        Result = if
          HardDriveNotFull andalso SavedPayload =/= "Payload Is To Long" andalso PushResult ->
            database:insert_dyn_table(FoundAppId, TopicId, SchemaId, SavedPayload, RequestTime),
            true;
          true andalso PushResult ->
            tools:log("info", io_lib:format("Tried to Push Messages to App: ~p Hard Drive is Full", [AppId])),
            false;
          true ->
            spawn(fun() ->
              mnesia:dirty_update_counter({counter_consumed, all}, 1),
              mnesia:dirty_update_counter({counter_consumed, FoundAppId}, 1)
            end),
            true
        end,
        Result
    end,
    CaseResult
  end, Apps),
  % TODO: is this a duplidate from above?
  % Looks like Appis being added Two
  database:add_published_counter(AppId),
  Success = length(lists:filter(fun(X) -> X =:= true end, R)),
  Failed = length(lists:filter(fun(X) -> X =:= false end, R)),
  if
    0 =:= Failed ->
      { 200, #{
          status => 'good',
          subscriber_Result => #{
               success => Success,
               failed => Failed
          }
        }
      };
    true ->
      { 207, #{
          status => 'mix',
          subscriber_Results => #{
               success => Success,
               failed => Failed
          }
        }
      }
  end.

% Internal Functions
pushMessages(_, _, _, _, 0) ->
   true;
pushMessages(_, _, _, _, true) ->
   false;
pushMessages(PushUrl, PushStatusCode, Headers, Payload, PushRetries) ->
   Request = {PushUrl, Headers, "application/json", Payload},
   StatusCode = pushCheck(httpc:request(post, Request, [], [])),
   if
      StatusCode =:= PushStatusCode ->
        pushMessages(PushUrl, PushStatusCode, Headers, Payload, true);
      true ->
        pushMessages(PushUrl, PushStatusCode, Headers, Payload, (PushRetries-1))
   end.

pushCheck({_,{{_,StatusCode,_},_,_}}) ->
   StatusCode;
pushCheck(_) ->
   false.
