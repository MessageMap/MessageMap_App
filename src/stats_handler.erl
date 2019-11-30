%%%-------------------------------------------------------------------
%%% @author Benjamin Adams
%%% @copyright (C) 2017, Ethan Solutions
%%% @doc
%%%  Return application status to the UI
%%% @end
%%% Created : 8. Sept 2017
%%%-------------------------------------------------------------------
-module(stats_handler).

-export([init/2]).

init(Req, Opts) ->
  { Claims, Req2 } = tools:verifyAuth(Req),
  % redirect if Claims = Bad
  AppId = cowboy_req:binding(appId, Req),
  Result = buildResponse(AppId),
  ReqFinal = cowboy_req:reply(200, tools:resp_headers(),
      jiffy:encode(Result),
      Req2),
  {ok, ReqFinal, Opts}.

% Internal functions
buildResponse(AppId) ->
  App = erlang:binary_to_list(AppId),
  Tbl = database:check_dyn_table(App),
  PubPull = mnesia:dirty_read({counter_published, Tbl}),
  Pub = resultConversion(PubPull),
  SubPull = mnesia:dirty_read({counter_consumed, Tbl}),
  Sub = resultConversion(SubPull),
  Size = mnesia:table_info(Tbl, size),
  Storage = database:table_storage_size(Tbl),
  #{
    id => AppId,
    messages_waiting => Size,
    published_messages => binary:list_to_bin(Pub),
    consumed_messages => binary:list_to_bin(Sub),
    storage => Storage
  }.

resultConversion([]) ->
  ["0"];
resultConversion([{_,_,Result}]) ->
  lists:flatten(io_lib:format("~p", [Result])).
