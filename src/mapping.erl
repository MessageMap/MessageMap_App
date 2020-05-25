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
    {Data} = FullData,
    io:format("Data: ~p~n", [Data]),
    if
        length(PathElements) > 0 ->
            Element = lists:nth(1, PathElements),
            TKeys = proplists:get_keys(Data),
            SearchResult = lists:member(Element, TKeys),
            SubPathElements = lists:delete(Element, PathElements),
            if
                SearchResult /= false ->
                    { NewData } = proplists:get_value(Element, Data),
                    if
                        length(PathElements) == 1 ->
                            io:format("ELEMENT: ~n~p~n", [SubPathElements]),
                            FinalElement = lists:nth(1, SubPathElements),
                            NewValue = runNewAction(binary:bin_to_list(Action), FinalElement, NewData),
                            NewResult = proplists:delete(FinalElement, NewData),
                            %io:format("Data: ~n~p~n FinalElement: ~n~p~n NewValue: ~n~p~n NewResult: ~n~p~n", [NewData, FinalElement, NewValue, NewResult]),
                            { lists:merge(Data, NewValue) };
                        true ->
                            LoopData = proplists:delete(Element, Data),
                            io:format("--- ~n~p~n", [Element]),
                            { lists:merge(LoopData, filter(Action, SubPathElements, {NewData}) ) }
                    end;
                true ->
                    {Data}
            end;
        true ->
            {Data}
    end.


%processDataType(Data, Filter, Elements) ->
%    if
%      is_tuple(Data) ->
%        { SubData } = Data,
%        TKeys = proplists:get_keys(SubData),
%        [ convertFilter({Filter}, proplists:get_value(Key, SubData)) || Key <- TKeys ];
% %       {Data, Filter};
%      is_list(Data) ->
%        [ processDataType(X, Filter, []) || X <- Data];
%        {Data, Filter};
%      true ->
%        Elements
%    end.

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
        % TODO ADD LOOP IF VALUE IS MAP OR LIST Searching!!!
        %[ processDataType(proplists:get_value(X, SubData), Filter, []) || X <- Key_list ],
        io:format("Was Not found~n", []),
        SubData
    end,
    %io:format("Orignal: ~n~p~n", [Data]),
    %io:format("Result: ~n~p~n", [{ActionResult}]),
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
