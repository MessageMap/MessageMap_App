%%%-------------------------------------------------------------------
%%% @author Benjamin Adams
%%% @copyright (C) 2020, MessageMap LLC
%%% @doc
%%%  This File will control Message Tabling System
%%%  CRUSH The 2GB Limit for tables in Erlang
%%% @end
%%% Created : 06. Jun 2020 9:08 AM
%%%-------------------------------------------------------------------
-module(database_manager).

%% API
-export([init/0]).
-export([createMsgsTbl/1]).
%-export([selectTblName/1]).
%-export([deleteTblName/1]).
%-export([allTblNames/1]).
-export([allTblNames/1]).

%debug
-export([addCreateTblMemory/3]).
% List of Functions to Export
-include_lib("stdlib/include/qlc.hrl").
-include("db/datatables.hrl").

% On boot Setup
init() ->
  io:format("Loading INIT for DB Manager", []),
  mnesia:load_textfile("db/messagetables.hrl"),
  AllApps = database:getAllAppDB(),
  io:format(" TODO: Load current apps and table structure into tbl ~nAllApps: ~p~n", [AllApps]),
  AppId = "a6cc9f6b-a146-44a4-b981-4ca0dc33953b", %uuid:to_string(uuid:uuid4()),
  io:format("result: ~p~n", [createMsgsTbl(AppId)]),
  true.

createMsgsTbl(AppId) ->
  io:format("Loaded AppID: ~p~n", [AppId]),
  Results = pullTblMemory(AppId),
  io:format("results~p~n", [Results]),
  Nodes = [node()],
  {TblName, CountResult} = if
    Results =:= [] ->
      Tbl = list_to_atom("msgs"++string:join(string:tokens(AppId, "-"),"")++"_0"),
      {Tbl, [0]};
    true ->
      [{_,AppId,Counters,_}] = Results,
      [Current|_] = lists:reverse(lists:sort(Counters)),
      Tbl = list_to_atom("msgs"++string:join(string:tokens(AppId, "-"), "")++"_"++erlang:integer_to_list(Current+1)),
      { Tbl, lists:merge(lists:sort(Counters), [Current+1]) }
  end,
  createTbl(TblName),
  addCreateTblMemory(AppId, CountResult, Nodes).

allTblNames(AppId) ->
  [{tblManager,AppId,Counters,_}] = pullTblMemory(AppId),
  [ list_to_atom("msgs"++string:join(string:tokens(AppId, "-"),"")++"_"++erlang:integer_to_list(C)) || C <- Counters].

% Internal Functions
addCreateTblMemory(AppId, Counter, Nodes) ->
  InternalResult = pullTblMemory(AppId),
  io:format("Internal Result: ~p~n", [InternalResult]),
  INS = fun() ->
      mnesia:write(#tblManager{appid=AppId, counter=Counter,nodes=Nodes})
      end,
  mnesia:sync_transaction(INS).

pullTblMemory(AppId) ->
  PULL = fun() ->
    Query = qlc:q([X || X <- mnesia:table(tblManager),
      X#tblManager.appid =:= AppId]),
    qlc:e(Query)
  end,
  {atomic, Results} = mnesia:sync_transaction(PULL),
  Results. 

createTbl(Tbl) ->
  mnesia:create_table(Tbl,
          [
            {type, ordered_set},
            {attributes, [rowId, pubId, topicId, schemaId, payload, createdOn]},
            {disc_copies, [node()]}
          ]).

% Create file db/messagetables.hrl
% Content
% {tables,
%   [{tblManager,
%     [{ attributes, [appid, counter, nodes] }]
%    }]
%  }
%  This will create tblManager as an in memory table
% Fill Table
% - Get list of Applications that have been created
% - Get List of current tables
% - For each table with msg
%  get Index of each:
%  TODO: Change database:check_dyn_table to call create table in here
%  New name: list_to_atom("msgs"++string:join(string:tokens(Name, "-"),"")++"_{NextID from Counter/(0 First)})

% Check Database Size:
% file:read_file_info("/var/messageMap/msgsf30c2ffb90a047a68c083939afee7a05.DCD").
% {ok,{file_info,2404983,regular,read_write,
% {{2020,6,5},{22,23,29}},
% {{2020,6,5},{22,23,30}},
% {{2020,6,5},{22,23,30}},
%   33188,1,1024,863688,208323,0,0}}
% -----
% {_,{_,sizeBytes,_,_,_,_,_,_,_,_,_,_,_,_} =  list_to_atom("msgs"++string:join(string:tokens(Name, "-"),"")++"_{Counter})
% Max sizeBytes 2 GB (2147483648)
%%%% NOTE Look at also: database:table_storage_size

% Check Hard Drive Storage: --FUNCTION Move to tools.erl
% TODO: Add to appmanager:bootup (stop for: application:stop(os_mon) )
%  - Startup  application:start(os_mon).
% Get HD Status:
% [{_,_,PercentFull}, _ } = disksup:get_disk_data().
% Calculate to full (Chop 10% off)

% TODO: Workflows -- Will be bouncing to and from database module for Tbl Names
% On Create APP (createMsgTable(appid, counter=0))
%  - Validate Table is not already Created with appid, counter
%  - Create storage table 0
%  - On create table write to :mem table: tblManager
% On Write Message
%  - Get Largest counter in array from: tblManager { appid, [counter] }
%  - Write to Table
%  - Async: Check Table size Write If >= Max Function CreateNextTbl
%  --       Call: createMsgTable(appid, counter+1)
% On Read Messages
%  - One Pull current Counter (min counter for app)
%  -
