%%%-------------------------------------------------------------------
%%% @author Ben Adams
%%% @copyright (C) 2017, MessageMap.io
%%% @doc
%%% Websocket to stream information back to client browser
%%% @end
%%% Created : 10, Nov 2017
%%%-------------------------------------------------------------------
-module(mapping).
-export([msgMapper/2]).
-export([test/0]).

runNewAction("remove", Key, Data) ->
    proplists:delete(Key, Data);
runNewAction("masking", Key, Data) ->
    NewData = proplists:delete(Key, Data),
    lists:merge(NewData, [{Key, <<"************">>}]);
runNewAction("rename", Elements, Data) ->
    OldName = lists:nth(1, Elements),
    NewName = lists:nth(2, Elements),
    NewData = maps:put(NewName, maps:get(OldName, Data), Data),
    maps:remove(OldName, NewData).

filter(Action, PathElements, FullData) ->
    { Data } = FullData,
    if
        is_list(Data) =:= false ->
            [ FinalElement ] = PathElements,
            { SubData } = Data,
            runNewAction(binary:bin_to_list(Action), FinalElement, SubData);
        length(PathElements) > 0 ->
            Element = lists:nth(1, PathElements),
            TKeys = proplists:get_keys(Data),
            SearchResult = lists:member(Element, TKeys),
            SubPathElements = lists:delete(Element, PathElements),
            if
                SearchResult /= false ->
                    NewData = proplists:get_value(Element, Data),
                    LoopData = proplists:delete(Element, Data),
                    SubResult = [ { Element, { filter(Action, SubPathElements, {NewData}) } } ],
                    { lists:merge(LoopData, SubResult) };
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
		  		 lists:map(fun(X) ->
             SubListing = proplists:get_value(X, Data),
             if
               is_tuple(SubListing) ->
                 {NewSub}  = SubListing,
                 findPath(NewSub, Key, lists:merge(Path, [X]));
               true ->
                 []
             end
             end, KeyList );
			  true ->
				   lists:flatten(Path)
      end;
    true ->
      lists:flatten(Path)
  end.

test() ->
  Data = {[{<<"checked">>,false},
  {<<"dimensions">>,{[{<<"width">>,5},{<<"height">>,10}]}},
  {<<"id">>,1},
  {<<"name">>,<<"A green door">>},
  {<<"price">>,12.5},
  {<<"tags">>,[<<"home">>,<<"green">>]}]},
  Key = <<"width">>,
  {SubData} = Data,
  Action = <<"masking">>,
  Path = lists:flatten(findPath(SubData, Key, [])),
  io:format("Testing!!!!~n Data: ~p~n key: ~p~n Path: ~p~n", [Data, Key, lists:flatten(Path)]),
  Result = filter(Action, Path, Data),
  io:format("Result: ~n~p~n", [Result]).

convertFilter({Filter}, {SubData}) ->
    Type = proplists:get_value(<<"type">>, Filter), 
    Value = proplists:get_value(<<"value">>, Filter), 
    %Result = findKey(binary:bin_to_list(Value), SubData),
    Key_list = proplists:get_keys(SubData),
    KeyFound = lists:member(Value, Key_list),
    ActionResult = if
      KeyFound ->
        runNewAction(binary:bin_to_list(Type), Value, SubData);
      true ->
        io:format("__________~n~p~n~p~n", [SubData, Value]),
        Path = lists:flatten(findPath(SubData, Value, [])),
        { Result } = filter(Type, Path, { SubData }),
        Result
    end,
    io:format("Result: ~n~p~n", [{ActionResult}]),
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
