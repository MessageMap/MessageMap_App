%%%-------------------------------------------------------------------
%%% @author Ben Adams
%%% @copyright (C) 2017, MessageMap.io
%%% @doc
%%% Websocket to stream information back to client browser
%%% @end
%%% Created : 10, Nov 2017
%%%-------------------------------------------------------------------
-module(ws_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, Opts)->
    {cowboy_websocket, Req, Opts}.

websocket_init(State) ->
    {[], State}.

websocket_handle(_, State) ->
    Data = pullData(),
    erlang:start_timer(10000, self(), jiffy:encode(Data)),
    {[{text, jiffy:encode(Data)}], State}.

websocket_info({timeout, _Ref, _}, State) ->
    Data = pullData(),
    erlang:start_timer(10000, self(), jiffy:encode(Data)),
    {[{text, jiffy:encode(Data)}], State};
websocket_info(_Info, State) ->
    {[], State}.

%Internal functions
pullData() ->
  AllApps = database:getAllAppDB(),
  AppStats = lists:map(fun(A) ->
    {_,App,AppName,_,_,_,_,_} = A,
    Tbl = database:check_dyn_table(App),
    PubPull = mnesia:dirty_read({counter_published, Tbl}),
    Pub = resultConversion(PubPull),
    SubPull = mnesia:dirty_read({counter_consumed, Tbl}),
    Sub = resultConversion(SubPull),
    Size = mnesia:table_info(Tbl, size),% * erlang:system_info(wordsize)), % / (1024*1024),
    Storage = database:table_storage_size(Tbl),
    #{
      id => binary:list_to_bin(App),
      name => binary:list_to_bin(AppName),
      messages_waiting => Size,
      published_messages => binary:list_to_bin([Pub]),
      consumed_messages => binary:list_to_bin([Sub]),
      storage => Storage
    }
  end, AllApps),
  [{_,_,Published}] = mnesia:dirty_read({counter_published, all}),
  [{_,_,Consumed}] = mnesia:dirty_read({counter_consumed, all}),
  {[{<<"applications">>,mnesia:table_info(applications, size)},
    {<<"topics">>,mnesia:table_info(topics, size)},
    {<<"schemas">>,mnesia:table_info(tblschemas, size)},
    {<<"totalMessagesPublished">>,Published},
    {<<"totalMessagesConsumed">>,Consumed},
    {<<"appstats">>,AppStats}]}.

%Internal functions
resultConversion([]) ->
  ["0"];
resultConversion([{_,_,Result}]) ->
  R = lists:flatten(io_lib:format("~p", [Result])),
  R.
