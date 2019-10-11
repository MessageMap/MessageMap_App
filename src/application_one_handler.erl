%%%-------------------------------------------------------------------
%%% @author Benjamin Adams
%%% @copyright (C) 2017, Message.io
%%% @doc
%%%  Return One application for listing
%%% @end
%%% Created : 8. Sept 2017
%%%-------------------------------------------------------------------
-module(application_one_handler).

-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {}).

init(_, Req, _Opts) ->
  {ok, Req, #state{}}.

handle(Req, State=#state{}) ->
  { Claims, Req2 } = tools:verifyAuth(Req),
  { Method, _ } = cowboy_req:method(Req),
  { AppId, _} = cowboy_req:binding(appId, Req),
  Result = processRequest(Method, Claims, AppId, Req),
  { ok, ReqFinal } = cowboy_req:reply(200, tools:resp_headers(),
      Result,
      Req2),
  {ok, ReqFinal, State}.

terminate(_Reason, _Req, _State) ->
  ok.

% Internal functions
processRequest(<<"GET">>, _, AppId, _) ->
  AppData = database:getAppDBAppId(binary:bin_to_list(AppId)),
  Result = buildResponse(element(1, list_to_tuple(AppData))),
  jiffy:encode(Result);
processRequest(<<"PUT">>, _, AppId, Req) ->
  {ok, Body, _} = cowboy_req:body_qs(Req),
  {_, Name} = lists:keyfind(<<"name">>, 1, Body),
  { _, Description } = lists:keyfind(<<"description">>, 1, Body),
  { _, OwnedTopics } = lists:keyfind(<<"ownedTopics">>, 1, Body),
  { _, SubscribedTopics } = lists:keyfind(<<"subscribedTopics">>, 1, Body),
  { _, AppData } = database:updateAppDBAppId(binary:bin_to_list(AppId), Name, Description, OwnedTopics, SubscribedTopics),
  Result = buildResponse(element(1, list_to_tuple(AppData))),
  jiffy:encode(Result);
processRequest(<<"DELETE">>, _, AppId, _) ->
  database:deleteAppDBAppId(binary:bin_to_list(AppId)),
  jiffy:encode([]).

% %% Internal functions
buildResponse(Data) ->
  {_,Id,Name,Description,ApiKeys,Ownedtopics,SubscribedTopics,CreatedOn} = element(1, list_to_tuple([Data])),
  #{
    id => binary:list_to_bin(Id),
    name => binary:list_to_bin(Name),
    description => binary:list_to_bin(Description),
    apiKeys => binary:list_to_bin(ApiKeys),
    ownedTopics => binary:list_to_bin(Ownedtopics),
    subscribedTopics => binary:list_to_bin(SubscribedTopics),
    createdOn => binary:list_to_bin(tools:convertDateTime(CreatedOn))}.
