%%%-------------------------------------------------------------------
%%% @author Ben Adams
%%% @copyright (C) 2017, MessageMap.io
%%% @doc
%%% Module that handles database interactions
%%% @end
%%% Created : 31. Aug 2017
%%%-------------------------------------------------------------------
-module(database).

%Import mnesia data schemas
-include_lib("stdlib/include/qlc.hrl").
-include("db/datatables.hrl").

-export([init/1]).
-export([storeDB/4]).
-export([setup_admin/0]).
-export([check_dyn_table/1]).
-export([getDB/1]).
-export([saveApp/5]).
-export([getAppDB/1]).
-export([getAppDBAppId/1]).
-export([getAllUsers/0]).
-export([get_dyn_table/2]).
-export([insert_dyn_table/5]).
-export([getAllAppDB/0]).
-export([deleteAppDBAppId/1]).
-export([updateAppDBAppId/6]).
-export([login/2]).
-export([saveSchema/2]).
-export([getSchemaDBSchemaId/1]).
-export([getAllSchemaDB/0]).
-export([updateSchemaDBSchemaId/3]).
-export([deleteSchemaDBSchemaId/1]).
-export([deleteTopicDBTopicId/1]).
-export([deleteDBUser/1]).
-export([async_dyn_delete/2]).
-export([saveTopic/3]).
-export([table_storage_size/1]).
-export([getTopicDB/1]).
-export([getAllTopicDB/0]).
-export([getTopicDBTopicId/1]).
-export([updateTopicDBTopicId/4]).
-export([validateAppDBAppIdApiKey/2]).
-export([status/1]).

-define(MNESIA_DIR, os:getenv("MM_MNESIA_DIR")).

init(Config) ->
  tools:log("info", io_lib:format("TODO: Parse Config for Default Password: ~p", [Config])),
  mnesia:stop(),
  % Set Database Location folder
  file:make_dir(?MNESIA_DIR),
  application:set_env(mnesia, dir, ?MNESIA_DIR),
  mnesia:create_schema([node()]), %TODO: Dynamicly add new nodes
  mnesia:start(),
  timer:seconds(15),
  try
    mnesia:table_info(type, organization),
    mnesia:table_info(type, accounts),
    mnesia:table_info(type, applications),
    mnesia:table_info(type, tblschemas),
    mnesia:table_info(type, topics),
    mnesia:table_info(type, accounts),
    mnesia:table_info(type, counter_published),
    mnesia:table_info(type, counter_consumed)
  catch
    exit: _ ->
      tools:log("info", io_lib:format("Need to create messages tables", [])),
      mnesia:create_table(organization,
        [
          {attributes, [id, name, address, plan]},
          {disc_copies, [node()]}
        ]),
      mnesia:create_table(accounts,
        [
          {attributes, [name, orgid, email, permissions, password,createdOn]},
          {disc_copies, [node()]}
        ]),
      mnesia:create_table(applications,
        [
          {attributes, [id,name,description,apikeys,ownedTopics,subscribedTopics,createdOn,encrypt]},
          {disc_copies, [node()]}
        ]),
      mnesia:create_table(tblschemas,
        [
          {attributes, [id,validation,version,createdOn]},
          {disc_copies, [node()]}
        ]),
      mnesia:create_table(topics,
        [
          {attributes, [id,name,description,schemaId,createdOn]},
          {disc_copies, [node()]}
        ]),
      mnesia:create_table(counter_published,
        [
          {attributes, record_info(fields, counter_published)}, % Try to Change others to this method
          {disc_copies, [node()]}
        ]),
      mnesia:create_table(counter_consumed,
        [
          {attributes, record_info(fields, counter_consumed)},
          {disc_copies, [node()]}
        ]),
      % Set all counters
      timer:apply_after(2000, mnesia, dirty_update_counter, [{counter_published, all}, 0]),
      timer:apply_after(2000, mnesia, dirty_update_counter, [{counter_consumed, all}, 0]),
      %% TODO Only run if first run
      setup_admin()
  end.

setup_admin() ->
  %TODO: Change to load from config file
  database:storeDB("MessageMap", "info@messagemap.io", ["Admin"], "$than#dams4292!"),
  database:storeDB("admin", "admin", ["Admin"], "MessageMap123").

%%%%%%%%%%%%%% topics
saveSchema(Validation, Version) ->
  Id = uuid:to_string(uuid:uuid4()),
  INS = fun() ->
    CreatedOn = calendar:universal_time(),
    mnesia:write(#tblschemas{id=Id,
                  validation=Validation,
                  version=Version,
                  createdOn=CreatedOn})
    end,
  mnesia:sync_transaction(INS),
  getSchemaDBSchemaId(Id).

getSchemaDBSchemaId(SchemaId) ->
  PULL = fun() ->
    Query = qlc:q([X || X <- mnesia:table(tblschemas),
      X#tblschemas.id =:= SchemaId]),
    qlc:e(Query)
  end,
  {atomic, Results} = mnesia:sync_transaction(PULL),
  Results.

getAllSchemaDB() ->
  PULL = fun() -> mnesia:select(tblschemas,[{'_',[],['$_']}]) end,
  {atomic, Results} = mnesia:sync_transaction(PULL),
  Results.

deleteSchemaDBSchemaId(SchemaId) ->
  DELETE = fun() ->
    [Obj_to_del] = getSchemaDBSchemaId(SchemaId),
    mnesia:delete_object(tblschemas, Obj_to_del, write)
  end,
  mnesia:sync_transaction(DELETE).

updateSchemaDBSchemaId(SchemaId, Validation, Version) ->
  Update = fun() ->
    [Obj_to_del] = getSchemaDBSchemaId(SchemaId),
    mnesia:delete_object(tblschemas, Obj_to_del, write),
    INS = fun() ->
      CreatedOn = calendar:universal_time(),
      mnesia:write(#tblschemas{id=SchemaId,
                    validation=binary:bin_to_list(Validation),
                    version=binary:bin_to_list(Version),
                    createdOn=CreatedOn})
      end,
    mnesia:sync_transaction(INS),
    getSchemaDBSchemaId(SchemaId)
  end,
  mnesia:transaction(Update).

%%%%%%%%%%%%%% topics
saveTopic(TopicName, Description, SchemaId) ->
  IsFound = getTopicDB(string:to_lower(TopicName)),
  INS = fun() ->
    CreatedOn = calendar:universal_time(),
    mnesia:write(#topics{id=uuid:to_string(uuid:uuid4()),
                  name=string:to_lower(TopicName),
                  description=Description,
                  schemaId=SchemaId,
                  createdOn=CreatedOn})
    end,
  if
    IsFound =:= [] ->
      mnesia:sync_transaction(INS),
      getTopicDB(string:to_lower(TopicName));
    true ->
      IsFound
  end.

getTopicDB(TopicName) ->
  PULL = fun() ->
    Query = qlc:q([X || X <- mnesia:table(topics),
      X#topics.name =:= string:to_lower(TopicName)]),
    qlc:e(Query)
  end,
  {atomic, Results} = mnesia:sync_transaction(PULL),
  Results.

getTopicDBTopicId(TopicId) ->
  PULL = fun() ->
    Query = qlc:q([X || X <- mnesia:table(topics),
      X#topics.id =:= TopicId]),
    qlc:e(Query)
  end,
  {atomic, Results} = mnesia:sync_transaction(PULL),
  Results.

getAllTopicDB() ->
  PULL = fun() -> mnesia:select(topics,[{'_',[],['$_']}]) end,
  {atomic, Results} = mnesia:sync_transaction(PULL),
  Results.

deleteTopicDBTopicId(TopicId) ->
  DELETE = fun() ->
    [Obj_to_del] = getTopicDBTopicId(TopicId),
    mnesia:delete_object(topics, Obj_to_del, write)
  end,
  mnesia:sync_transaction(DELETE).

updateTopicDBTopicId(TopicId, Name, Description, SchemaId) ->
  Update = fun() ->
    [Obj_to_del] = getTopicDBTopicId(TopicId),
    mnesia:delete_object(topics, Obj_to_del, write),
    INS = fun() ->
      CreatedOn = calendar:universal_time(),
      mnesia:write(#topics{id=TopicId,
                    name=binary:bin_to_list(Name),
                    description=binary:bin_to_list(Description),
                    schemaId=binary:bin_to_list(SchemaId),
                    createdOn=CreatedOn})
      end,
    mnesia:sync_transaction(INS),
    getTopicDBTopicId(TopicId)
  end,
  mnesia:sync_transaction(Update).

%%%%%%%%%%%%%% applications
saveApp(AppName, AppDescription, AppOwnedTopics, AppSubscribedTopics, Encrypt) ->
  IsFound = getAppDB(string:to_lower(AppName)),
  INS = fun() ->
    CreatedOn = calendar:universal_time(),
    mnesia:write(#applications{id=uuid:to_string(uuid:uuid4()),
                  name=string:to_lower(AppName),
                  description=AppDescription,
                  apikeys=uuid:to_string(uuid:uuid4()),
                  ownedTopics=AppOwnedTopics,
                  subscribedTopics=AppSubscribedTopics,
                  encrypt=Encrypt,
                  createdOn=CreatedOn})
    end,
  if
    IsFound =:= [] ->
      mnesia:sync_transaction(INS),
      getAppDB(string:to_lower(AppName));
    true ->
      IsFound
  end.

getAppDB(AppName) ->
  PULL = fun() ->
    Query = qlc:q([X || X <- mnesia:table(applications),
      X#applications.name =:= string:to_lower(AppName)]),
    qlc:e(Query)
  end,
  {atomic, Results} = mnesia:sync_transaction(PULL),
  Results.

getAppDBAppId(AppId) ->
  PULL = fun() ->
    Query = qlc:q([X || X <- mnesia:table(applications),
      X#applications.id =:= AppId]),
    qlc:e(Query)
  end,
  {atomic, Results} = mnesia:sync_transaction(PULL),
  Results.

validateAppDBAppIdApiKey(AppId, Apikey) ->
  PULL = fun() ->
    Query = qlc:q([X || X <- mnesia:table(applications),
      X#applications.id =:= AppId,
      X#applications.apikeys =:= Apikey]),
    qlc:e(Query)
  end,
  {atomic, Results} = mnesia:sync_transaction(PULL),
  R = case Results of
    [] ->
      #{};
    _ ->
      OneResult = lists:last(Results),
      #{
        id => OneResult#applications.id,
        name => OneResult#applications.name,
        ownedTopics => OneResult#applications.ownedTopics,
        subscribedTopics => OneResult#applications.subscribedTopics,
        encrypt => OneResult#applications.encrypt
      }
  end,
  io:format("~p~n", [R]),
  R.

getAllAppDB() ->
  PULL = fun() -> mnesia:select(applications,[{'_',[],['$_']}]) end,
  {atomic, Results} = mnesia:sync_transaction(PULL),
  Results.

deleteAppDBAppId(AppId) ->
  DELETE = fun() ->
    [Obj_to_del] = getAppDBAppId(AppId),
    mnesia:delete_object(applications, Obj_to_del, write)
  end,
  mnesia:sync_transaction(DELETE).

updateAppDBAppId(AppId, Name, Description, OwnedTopics, SubscribedTopics, Encrypt) ->
  Update = fun() ->
    [Obj_to_del] = getAppDBAppId(AppId),
    {_,_,_,_,ApiKeys,_,_,_,_} = Obj_to_del,
    mnesia:delete_object(applications, Obj_to_del, write),
    INS = fun() ->
      CreatedOn = calendar:universal_time(),
      mnesia:write(#applications{id=AppId,
                    name=binary:bin_to_list(Name),
                    description=binary:bin_to_list(Description),
                    apikeys=ApiKeys,
                    ownedTopics=binary:bin_to_list(OwnedTopics),
                    subscribedTopics=binary:bin_to_list(SubscribedTopics),
                    createdOn=CreatedOn,
                    encrypt=Encrypt})
      end,
    mnesia:sync_transaction(INS),
    getAppDBAppId(AppId)
  end,
  mnesia:transaction(Update).

deleteDBUser(Email) ->
 DELETE = fun() ->
   [Obj] = getDB(Email),
   mnesia:delete_object(accounts, Obj, write)
 end,
 mnesia:sync_transaction(DELETE).

storeDB(Name, Email, Permissions, Password) ->
  IsFound = getDB(string:to_lower(Email)),
  INS = fun() ->
    CreatedOn = calendar:universal_time(),
    mnesia:write(#accounts{name=Name,
                  email=string:to_lower(Email),
                  permissions=Permissions,
                  password=encryption:create(Password),
                  createdOn=CreatedOn})
    end,
  if
    IsFound =:= [] ->
      mnesia:sync_transaction(INS);
    true ->
      false
  end.

getDB(Email) ->
  PULL = fun() ->
    Query = qlc:q([X || X <- mnesia:table(accounts),
      X#accounts.email =:= string:to_lower(Email)]),
    qlc:e(Query)
  end,
  {atomic, Results} = mnesia:sync_transaction(PULL),
  Results.

getAllUsers() ->
  PULL = fun() ->
    Query = qlc:q([[X#accounts.name] || X <- mnesia:table(accounts),
      string:to_lower(X#accounts.name) =/= string:to_lower("admin"),
      string:to_lower(X#accounts.name) =/= string:to_lower("MessageMap")
    ]),
    qlc:e(Query)
         end,
  {atomic, Results} = mnesia:sync_transaction(PULL),
  Results.

login(Email, Password) ->
  PULL = fun() ->
    Query = qlc:q([ [ X#accounts.name, X#accounts.email, X#accounts.permissions ]  || X <- mnesia:table(accounts),
      X#accounts.email =:= string:to_lower(Email),
      success =:= encryption:validate(Password, X#accounts.password) ]),
    qlc:e(Query)
  end,
  {atomic, Results} = mnesia:sync_transaction(PULL),
  if
    Results =:= [] ->
      { false, false };
    true ->
      [[Name, VEmail, Permissions]] = Results,
      { true, encryption:ewtCreate(erlang:list_to_binary(Name), erlang:list_to_binary(VEmail), erlang:list_to_binary(Permissions)) }
  end.

%% Dynamic tables
check_dyn_table(Name) ->
  Tbl = list_to_atom("msgs"++string:join(string:tokens(Name, "-"),"")),
  try
    mnesia:table_info(Tbl, type)
  catch
    exit: _ ->
      mnesia:create_table(Tbl,
          [
            {attributes, [rowId, pubId, topicId, schemaId, payload, createdOn]},
            {disc_copies, [node()]}
          ])
  end,
  Tbl.

get_dyn_table(Tbl, Limit) ->
  PULL = fun() -> mnesia:first(Tbl,[{'_',[],['$_']}], Limit, read) end,
  {atomic, Results} = mnesia:sync_transaction(PULL),
  getResult(Tbl, Results).

getResult(_, '$end_of_table') ->
  [];
getResult(Tbl, UnFilteredResults) ->
  {Results, _ }  = UnFilteredResults,
  Payloads = [element(6,T) || T <- Results],
  spawn(database, async_dyn_delete, [Tbl, Results]),
  Payloads.

insert_dyn_table(Tbl, AppId, TopicId, SchemaId, Payload) ->
  RowId = uuid:to_string(uuid:uuid4()),
  CreatedOn = calendar:universal_time(),
  InsertData = {Tbl, RowId, AppId, TopicId, SchemaId, Payload,  CreatedOn},
  INS = fun() -> mnesia:write(InsertData) end,
  mnesia:sync_transaction(INS),
  %Update counters
  mnesia:dirty_update_counter({counter_published, all}, 1),
  PubTbl = check_dyn_table(AppId),
  mnesia:dirty_update_counter({counter_published, PubTbl}, 1), %TODO: Change to AppId or publishing application
  #{
    result => true
  }.

async_dyn_delete(Tbl, Results) ->
    DELETE = fun() ->
      lists:foreach(fun(Object) ->
        mnesia:dirty_update_counter({counter_consumed, all}, 1),
        mnesia:dirty_update_counter({counter_consumed, Tbl}, 1),
        {_, Key,_,_,_,_,_} = Object,
        mnesia:delete(Tbl, Key, write)
      end, Results)
    end,
    mnesia:sync_transaction(DELETE).

table_storage_size(Tbl) ->
  DCDSize = filelib:file_size(io_lib:format('~s/~s.DCD', [?MNESIA_DIR, Tbl])),
  DCLSize = filelib:file_size(io_lib:format('~s/~s.DCL', [?MNESIA_DIR, Tbl])),
  DCDSize+DCLSize.

status(Name) ->
  Tbl = list_to_atom("msgs"++string:join(string:tokens(Name, "-"),"")),
  Size = table_storage_size(Tbl),
  #{
    topic => Name,
    data_storage => Size
  }.
