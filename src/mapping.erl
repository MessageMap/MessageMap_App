%%%-------------------------------------------------------------------
%%% @author Ben Adams - Ben@MessageMap.IO
%%% @copyright (C) 2017-2022, MessageMap LLC
%%% @doc
%%% Mapping module to convert payload to required mapping value
%%% @end
%%% Created : 10, Nov 2019
%%%-------------------------------------------------------------------
-module(mapping).
-export([msgMapper/2]).

runNewAction("remove", Key, Data, _) ->
    proplists:delete(Key, Data);
runNewAction("masking", Key, Data, _) ->
    NewData = proplists:delete(Key, Data),
    lists:merge(NewData, [{Key, <<"************">>}]);
runNewAction("rename", Key, Data, NewName) ->
    Value = proplists:get_value(Key, Data),
    NewData = proplists:delete(Key, Data),
    lists:merge(NewData, [{NewName, Value}]).

filter(Action, PathElements, FullData, OldName) ->
    {Data} = FullData,
    if
        is_list(Data) =:= false ->
            [FinalElement] = PathElements,
            {SubData} = Data,
            runNewAction(binary:bin_to_list(Action), FinalElement, SubData, OldName);
        length(PathElements) > 0 ->
            Element = lists:nth(1, PathElements),
            TKeys = proplists:get_keys(Data),
            SearchResult = lists:member(Element, TKeys),
            SubPathElements = lists:delete(Element, PathElements),
            if
                SearchResult /= false ->
                    NewData = proplists:get_value(Element, Data),
                    LoopData = proplists:delete(Element, Data),
                    SubResult = [{Element, {filter(Action, SubPathElements, {NewData}, OldName)}}],
                    {lists:merge(LoopData, SubResult)};
                true ->
                    {Data}
            end;
        true ->
            {Data}
    end.

findPath(Data, Key, Path) ->
    if
        is_list(Data) ->
            KeyList = proplists:get_keys(Data),
            KeyFound = lists:member(Key, KeyList),
            if
                KeyFound ->
                    lists:merge(Path, [Key]);
                length(KeyList) > 0 ->
                    lists:map(
                        fun(X) ->
                            SubListing = proplists:get_value(X, Data),
                            if
                                is_tuple(SubListing) ->
                                    {NewSub} = SubListing,
                                    findPath(NewSub, Key, lists:merge(Path, [X]));
                                true ->
                                    []
                            end
                        end,
                        KeyList
                    );
                true ->
                    lists:flatten(Path)
            end;
        true ->
            lists:flatten(Path)
    end.

convertFilter({Filter}, {SubData}) ->
    Type = proplists:get_value(<<"type">>, Filter),
    Value = proplists:get_value(<<"value">>, Filter),
    CurrentName = proplists:get_value(<<"key">>, Filter, false),
    Key_list = proplists:get_keys(SubData),
    KeyFound =
        if
            CurrentName =:= false ->
                lists:member(Value, Key_list);
            true ->
                lists:member(CurrentName, Key_list)
        end,
    ActionResult =
        if
            KeyFound ->
                if
                    CurrentName =:= false ->
                        runNewAction(binary:bin_to_list(Type), Value, SubData, false);
                    true ->
                        runNewAction(binary:bin_to_list(Type), CurrentName, SubData, Value)
                end;
            true ->
                {Result} =
                    if
                        CurrentName =:= false ->
                            Path = lists:flatten(findPath(SubData, Value, [])),
                            filter(Type, Path, {SubData}, false);
                        true ->
                            Path = lists:flatten(findPath(SubData, CurrentName, [])),
                            filter(Type, Path, {SubData}, Value)
                    end,
                Result
        end,
    {ActionResult};
convertFilter(_, Data) ->
    Data.

msgMapper(Filter, Data) ->
    if
        length(Filter) > 0 ->
            msgMapper(
                lists:reverse(lists:droplast(lists:reverse(Filter))),
                convertFilter(lists:nth(1, Filter), Data)
            );
        true ->
            Data
    end.
