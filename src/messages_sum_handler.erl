%%%-------------------------------------------------------------------
%%% @author Benjamin Adams
%%% @copyright (C) 2017, MessageMap.io
%%% @doc
%%%  Return Total Number of Transfered Messages
%%% @end
%%% Created : 16. Aug 2017
%%%-------------------------------------------------------------------
-module(messages_sum_handler).

-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {}).

init(_, Req, _Opts) ->
  {ok, Req, #state{}}.

handle(Req, State=#state{}) ->
  [{_,_,Pub}] = mnesia:dirty_read({counter_published, all}),
  [{_,_,Sub}] = mnesia:dirty_read({counter_consumed, all}),
  Apps = length(database:getAllAppDB()), 
  Sum = lists:sum([Pub, Sub]),
  Users = mnesia:table_info(accounts, size)-1,
  Result = {[
                {<<"total_apps">>, Apps},
                {<<"total_messages">>,Sum},
                {<<"total_users">>,Users}
	]},
  {ok, Req2} = cowboy_req:reply(200, tools:resp_headers(),
    jiffy:encode(Result),
    Req),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.