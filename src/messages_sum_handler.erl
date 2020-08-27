%%%-------------------------------------------------------------------
%%% @author Benjamin Adams
%%% @copyright (C) 2020, MessageMap.io
%%% @doc
%%%  Return Total Number of Transfered Messages
%%% @end
%%% Created : 08 Jun 2020
%%%-------------------------------------------------------------------
-module(messages_sum_handler).

-export([init/2]).

init(Req, Opts) ->
  [{_,_,Pub}] = mnesia:dirty_read({counter_published, all}),
  [{_,_,Sub}] = mnesia:dirty_read({counter_consumed, all}),
  Apps = length(database:getAllAppDB()), 
  Sum = lists:sum([Pub, Sub]),
  [{_,_,Storage}, _ ] = disksup:get_disk_data(),
  Users = mnesia:table_info(accounts, size)-1,
  UserNames = lists:map(fun(K) -> binary:list_to_bin(K) end, lists:append(database:getAllUsers())),
  Result = {[
                {<<"total_apps">>, Apps},
                {<<"total_messages">>,Sum},
                {<<"total_users">>,Users},
                {<<"usernames">>, UserNames},
                {<<"storage">>, Storage}
	]},
  Req2 = cowboy_req:reply(200, tools:resp_headers(),
    jiffy:encode(Result),
    Req),
  {ok, Req2, Opts}.
