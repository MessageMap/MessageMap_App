%%%-------------------------------------------------------------------
%%% @author Ben Adams
%%% @copyright (C) 2017, MessageMap.io
%%% @doc
%%% Websocket to stream information back to client browser
%%% @end
%%% Created : 10, Nov 2017
%%%-------------------------------------------------------------------
-module(mapping).
-export([init/0]).
-export([msgMapper/2]).

findKey(Key, Data) ->
    if
        is_list(Data) ->
            false;
        true ->
            lists:search(fun(K) -> K == Key end, maps:keys(Data))
    end.

runAction("remove", Key, Data) ->
    Elements = string:tokens(Key, "=>"),
    io:format("Starting Remove~n~p~n~p~n", [Elements, Data]),
    Remove = lists:nth(1, Elements),
    NewData = maps:remove(Remove, Data),
    io:format("New Data: ~p~n", [NewData]),
    NewData;
runAction("mask", Key, Data) ->
    Elements = string:tokens(Key, "=>"),
    io:format("Starting Masking~n", []),
    Name = lists:nth(1, Elements),
    maps:update(Name, "***********", Data);
runAction("rename", Key, Data)->
    Elements = string:tokens(Key, "=>"),
    io:format("Starting Rename~n", []),
    OldName = lists:nth(1, Elements),
    NewName = lists:nth(2, Elements),
    NewData = maps:put(NewName, maps:get(OldName, Data), Data),
    maps:remove(OldName, NewData).

filter(Action, PathElements, Data) ->
    if
        length(PathElements) > 0 ->
            Element = lists:nth(1, PathElements),
            SearchResult = findKey(Element, Data),
            SubPathElements = lists:delete(Element, PathElements),
            if
                SearchResult /= false ->
                    NewData = maps:get(Element, Data),
                    if
                        length(SubPathElements) == 1 ->
                            if
                                is_list(NewData) ->
                                    UpdateData = [  checkArray(Action, lists:nth(1, SubPathElements), X) || X <- NewData ],
                                    maps:update(Element, UpdateData, Data);
                                true ->
                                    UpdateData = runAction(Action, lists:nth(1, SubPathElements), NewData),
                                    maps:update(Element, filter(Action, SubPathElements, UpdateData), Data)
                            end;
                        true ->
                            if
                                is_list(NewData) ->
                                    maps:update(Element, [ filter(Action, SubPathElements, X) || X <- NewData ], Data);
                                true ->
                                    maps:update(Element, filter(Action, SubPathElements, NewData), Data)
                            end
                    end;
                true ->
                    Data
            end;
        true ->
            Data
    end.

checkArray(Action, Element, Data) ->
    ListSearchResult = findKey(Element, Data),
    Result = if
        ListSearchResult /= false ->
            runAction(Action, Element, Data);
        true ->
            Data
    end,
    Result.

msgMapper(Filter, Data) ->
    if
        length(Filter) > 0 ->
            F = lists:nth(1, Filter),
            Elements = string:tokens(F, "^"),
            Action = lists:nth(1, Elements),
            Path = lists:nth(2, Elements),
            PathElements = string:tokens(Path, "/"),
            if
                length(PathElements) > 1 ->
                    NewData = filter(Action, PathElements, Data);
                true ->
                    NewData = runAction(Action, Path, Data)
            end,
            msgMapper(
                lists:reverse(lists:droplast(lists:reverse(Filter))),
                NewData
            );
        true ->
            Data
    end.

init() ->
    M_EX = #{
        "name" => "Ben",
        "age" => 41,
        "addr" => #{
            "st" => "Some Street",
            "town" => #{
                "name" => "Henrietta",
                "zip" => 14586
            }
        },
        "classes" => [
            #{
                "name" => "class1",
                "Moving" => [
                    #{"maskMe" => "Masking"}
                ]
            },
            #{
                "name" => "class2"
            }
        ]
    },
    Filter = [
        "rename^name=>NewName",
        "mask^age",
        "remove^addr/st",
        "remove^addr/town/zip",
        "mask^classes/Moving/maskMe"
    ],
    NewMap = msgMapper(Filter, M_EX),
    io:format("-----------------~nNew:~n~p~n", [NewMap]).