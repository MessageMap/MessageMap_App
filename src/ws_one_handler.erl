%%%-------------------------------------------------------------------
%%% @author Ben Adams
%%% @copyright (C) 2017, MessageMap.io
%%% @doc
%%% Websocket to stream information back to client browser,
%%% Only One Application
%%% @end
%%% Created : 10, Nov 2017
%%%-------------------------------------------------------------------
-module(ws_one_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, Opts)->
  {cowboy_websocket, Req, Opts}.

websocket_init(State) ->
  erlang:start_timer(1000, self(), <<"Welcome to MessageMap!">>),
  {[], State}.

websocket_handle({text, _}, State) ->
  Data = pullData(),
  erlang:start_timer(10000, self(), jiffy:encode(Data)),
  {[jiffy:encode(Data)], State};
websocket_handle(_Data, State) ->
  {[], State}.

websocket_info({timeout, _Ref, _}, State) ->
  Data = pullData(),
  erlang:start_timer(10000, self(), jiffy:encode(Data)),
  {[jiffy:encode(Data)], State};
websocket_info(_Info, State) ->
  {[], State}.

%Internal functions
pullData() ->
  [{_,_,Published}] = mnesia:dirty_read({counter_published, all}),
  [{_,_,Consumed}] = mnesia:dirty_read({counter_consumed, all}),
  {[{<<"applications">>,mnesia:table_info(applications, size)},
    {<<"topics">>,mnesia:table_info(topics, size)},
    {<<"schemas">>,mnesia:table_info(tblschemas, size)},
    {<<"totalMessagesPublished">>,Published},
    {<<"totalMessagesConsumed">>,Consumed}]}.
