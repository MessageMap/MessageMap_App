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

-behaviour(cowboy_http_handler).
-behaviour(cowboy_websocket_handler).
-export([init/3, handle/2, terminate/3]).
-export([
    websocket_init/3, websocket_handle/3,
    websocket_info/3, websocket_terminate/3
]).


init(_, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

handle(_Req, State) ->
    {ok, Req2} = cowboy_http_req:reply(404, [{'Content-Type', <<"text/html">>}]),
    {ok, Req2, State}.

websocket_init(_TransportName, Req, _Opts) ->
    {ok, Req, undefined_state}.

websocket_handle({text, _}, Req, State) ->
    { Id, _ } = cowboy_req:binding(id, Req),
    Data = pullData(Req, Id),
    erlang:start_timer(10000, self(), jiffy:encode(Data)),
    {reply, {text, jiffy:encode(Data)}, Req, State, hibernate };

websocket_handle(_Any, Req, State) ->
    {reply, {text, << "whut?">>}, Req, State, hibernate }.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
    Data = pullData(Req, []),
    erlang:start_timer(10000, self(), jiffy:encode(Data)),
    {reply, {text, Msg}, Req, State};

websocket_info(_Info, Req, State) ->
    {ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

terminate(_Reason, _Req, _State) ->
    ok.

%Internal functions
pullData(Req, Id) ->
  tools:log("debug", io_lib:format("Needed in ws one: Req: ~p Id: ~p", [Req, Id])),
  [{_,_,Published}] = mnesia:dirty_read({counter_published, all}),
  [{_,_,Consumed}] = mnesia:dirty_read({counter_consumed, all}),
  {[{<<"applications">>,mnesia:table_info(applications, size)},
    {<<"topics">>,mnesia:table_info(topics, size)},
    {<<"schemas">>,mnesia:table_info(tblschemas, size)},
    {<<"totalMessagesPublished">>,Published},
    {<<"totalMessagesConsumed">>,Consumed}]}.
