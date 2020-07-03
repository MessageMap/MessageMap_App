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
-export([recordCount/1]).
%-export([deleteTblName/1]).
%-export([allTblNames/1]).
-export([allTblNames/1]).
-export([insertTblName/1]).
-export([selectTblName/1]).

%debug
-export([addCreateTblMemory/3]).
-export([createTbl/1]).

%-define(Max_Table_Size, 1077483648).
-define(Max_Table_Size, 104857600).

% List of Functions to Export
-include_lib("stdlib/include/qlc.hrl").
-include("db/datatables.hrl").
-include_lib("kernel/include/file.hrl").

% On boot Setup
init() ->
  io:format("Loading INIT for DB Manager", []),
  mnesia:load_textfile("db/messagetables.hrl"),
  AllApps = database:getAllAppDB(),
  lists:foreach(fun(App) ->
      { _, AppId, _, _, _, _, _, _, _, _, _, _, _, _, _ } = App,
      StartTblName = list_to_atom("msgs"++string:join(string:tokens(AppId, "-"),"")),
      findTblsLike(StartTblName, AppId),
      ResultsIns = pullTblMemory(AppId),
      io:format("Tables Saved for: ~p~n -----~n~p~n", [AppId, ResultsIns])
    end,
    AllApps),
  true.

createMsgsTbl(AppId) ->
  Results = pullTblMemory(AppId),
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
  addCreateTblMemory(AppId, CountResult, Nodes),
  TblName.

insertTblName(AppId) ->
  Tbl_list = allTblNames(AppId),
  if
    Tbl_list =:= [] ->
      createMsgsTbl(AppId);
    true ->
      TblName = lists:nth(1, lists:reverse(lists:sort(Tbl_list))),
      % DCD file Ext for ordered_set
      %{_,{_,SizeBytes,_,_,_,_,_,_,_,_,_,_,_,_}} = file:read_file_info("/var/messageMap/"++erlang:atom_to_list(TblName)++".DAT"),
      SizeBytes = (mnesia:table_info(TblName,memory)*erlang:system_info(wordsize)),
      if
        SizeBytes > ?Max_Table_Size ->
          createMsgsTbl(AppId);
        true ->
          TblName
      end
  end.

selectTblName(AppId) ->
  Tbl_list = allTblNames(AppId),
  if
    Tbl_list =:= [] ->
      { false, 0 };
    true ->
      TblName = lists:nth(1, lists:sort(Tbl_list)),
      { TblName,  mnesia:table_info(TblName, size) }
  end.
      
recordCount(AppId) ->
  Tbl_list = allTblNames(AppId),
  if
    Tbl_list =:= [] ->
      0;
    true ->
      lists:foldl(fun(X, Sum) -> mnesia:table_info(X, size) + Sum end, 0, Tbl_list)
  end.

allTblNames(AppId) ->
  Tbl_check = pullTblMemory(AppId),
  if
    Tbl_check =:= [] ->
      [];
    true ->
      [{tblManager,AppId,Counters,_}] = Tbl_check,
      [ list_to_atom("msgs"++string:join(string:tokens(AppId, "-"),"")++"_"++erlang:integer_to_list(C)) || C <- Counters]
  end.

% Internal Functions
findTblsLike(TblName, AppId) ->
   {ok, Filenames} = file:list_dir("/var/messageMap/"),
   lists:foreach(fun(FileName) ->
       ListTblName = erlang:atom_to_list(TblName),
       Unique = length(string:tokens(FileName, "DCD")),
       tblFound(string:str(FileName, ListTblName), FileName, AppId, Unique)
     end,
   Filenames),
   true.

tblFound(1, FileName, AppId, 2) ->
    Counter = erlang:list_to_integer(lists:nth(2, string:tokens(lists:nth(1, string:tokens(FileName, ".")), "_"))),
    Results = pullTblMemory(AppId),
    Nodes = [node()],
    CountResult = if
      Results =:= [] ->
         [Counter];
      true ->
         [{_,AppId,Counters,_}] = Results,
         lists:usort(lists:merge(lists:sort(Counters), [Counter]))
    end,
    addCreateTblMemory(AppId, CountResult, Nodes);
tblFound(_, _, _, _) ->
    false.

addCreateTblMemory(AppId, Counter, Nodes) ->
  %InternalResult = pullTblMemory(AppId),
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
            {type, set},
            {attributes, record_info(fields, message)},
            {record_name, message},
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
