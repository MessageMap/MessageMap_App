%%%-------------------------------------------------------------------
%%% @author Ben Adams - Ben@MessageMap.IO
%%% @copyright (C) 2017-2022, MessageMap LLC
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
-export([deleteTblName/2]).
-export([deleteAppTables/1]).
-export([allTblNames/1]).
-export([insertTblName/1]).
-export([selectTblName/1]).

%debug
-export([addCreateTblMemory/3]).
-export([createTbl/1]).

-define(MNESIA_DIR, tools:configFile("data")).
-define(Max_Table_Size, 1077483648).
-define(NODE_LIST, tools:configFile("dataNodes")).

% List of Functions to Export
-include_lib("stdlib/include/qlc.hrl").
-include("db/datatables.hrl").
-include_lib("kernel/include/file.hrl").

% On boot Setup
init() ->
    mnesia:load_textfile("db/messagetables.hrl"),
    AllApps = database:getAllAppDB(),
    lists:foreach(
        fun(App) ->
            {_, AppId, _, _, _, _, _, _, _, _, _, _, _, _, _} = App,
            StartTblName = list_to_atom("msgs" ++ string:join(string:tokens(AppId, "-"), "")),
            findTblsLike(StartTblName, AppId),
            pullTblMemory(AppId)
        end,
        AllApps
    ),
    true.

createMsgsTbl(AppId) ->
    Results = pullTblMemory(AppId),
    CTable = tableChecking(Results),
    Nodes = [node()],
    {TblName, CountResult} =
        if
            CTable ->
                Tbl = list_to_atom("msgs" ++ string:join(string:tokens(AppId, "-"), "") ++ "_0"),
                {Tbl, [0]};
            true ->
                [{_, AppId, Counters, _}] = Results,
                [Current | _] = lists:reverse(lists:sort(Counters)),
                Tbl = list_to_atom(
                    "msgs" ++ string:join(string:tokens(AppId, "-"), "") ++ "_" ++
                        erlang:integer_to_list(Current + 1)
                ),
                {Tbl, lists:merge(lists:sort(Counters), [Current + 1])}
        end,
    createTbl(TblName),
    addCreateTblMemory(AppId, CountResult, Nodes),
    TblName.

tableChecking([]) ->
    true;
tableChecking([{_, _, [], _}]) ->
    true;
tableChecking(_) ->
    false.

deleteAppTables(AppId) ->
    lists:foreach(
        fun(Tbl) ->
            deleteTblName(Tbl, AppId)
        end,
        allTblNames(AppId)
    ).

deleteTblName(Tbl, AppId) ->
    Counter = erlang:list_to_integer(lists:nth(2, string:tokens(erlang:atom_to_list(Tbl), "_"))),
    ResultTbls = pullTblMemory(AppId),
    [{_, _, Counters, Nodes}] = ResultTbls,
    NewCounters = lists:filter(fun(X) -> X =/= Counter end, Counters),
    addCreateTblMemory(AppId, NewCounters, Nodes),
    mnesia:delete_table(Tbl).

insertTblName(AppId) ->
    Tbl_list = allTblNames(AppId),
    if
        Tbl_list =:= [] ->
            createMsgsTbl(AppId);
        true ->
            TblName = lists:nth(1, lists:reverse(lists:sort(Tbl_list))),
            SizeBytes = (mnesia:table_info(TblName, memory) * erlang:system_info(wordsize)),
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
            {false, 0};
        true ->
            TblName = lists:nth(1, lists:sort(Tbl_list)),
            {TblName, mnesia:table_info(TblName, size)}
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
            [{tblManager, AppId, Counters, _}] = Tbl_check,
            [
                list_to_atom(
                    "msgs" ++ string:join(string:tokens(AppId, "-"), "") ++ "_" ++
                        erlang:integer_to_list(C)
                )
             || C <- Counters
            ]
    end.

% Internal Functions
findTblsLike(TblName, AppId) ->
    {ok, Filenames} = file:list_dir(?MNESIA_DIR),
    lists:foreach(
        fun(FileName) ->
            ListTblName = erlang:atom_to_list(TblName),
            Unique = length(string:tokens(FileName, "DCD")),
            tblFound(string:str(FileName, ListTblName), FileName, AppId, Unique)
        end,
        Filenames
    ),
    true.

tblFound(1, FileName, AppId, 2) ->
    Counter = erlang:list_to_integer(
        lists:nth(2, string:tokens(lists:nth(1, string:tokens(FileName, ".")), "_"))
    ),
    Results = pullTblMemory(AppId),
    Nodes = ?NODE_LIST,
    CountResult =
        if
            Results =:= [] ->
                [Counter];
            true ->
                [{_, AppId, Counters, _}] = Results,
                lists:usort(lists:merge(lists:sort(Counters), [Counter]))
        end,
    addCreateTblMemory(AppId, CountResult, Nodes);
tblFound(_, _, _, _) ->
    false.

addCreateTblMemory(AppId, Counter, Nodes) ->
    %InternalResult = pullTblMemory(AppId),
    INS = fun() ->
        mnesia:write(#tblManager{appid = AppId, counter = Counter, nodes = Nodes})
    end,
    mnesia:sync_transaction(INS).

pullTblMemory(AppId) ->
    PULL = fun() ->
        Query = qlc:q([
            X
         || X <- mnesia:table(tblManager),
            X#tblManager.appid =:= AppId
        ]),
        qlc:e(Query)
    end,
    {atomic, Results} = mnesia:sync_transaction(PULL),
    Results.

createTbl(Tbl) ->
    mnesia:create_table(
        Tbl,
        [
            {type, set},
            {attributes, record_info(fields, message)},
            {record_name, message},
            {disc_copies, [node()]}
        ]
    ).