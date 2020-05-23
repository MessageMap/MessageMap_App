%%%-------------------------------------------------------------------
%%% @author Benjamin Adams
%%% @copyright (C) 2017, Message.io
%%% @doc
%%%  Return One application for listing
%%% @end
%%% Created : 8. Sept 2017
%%%-------------------------------------------------------------------
-module(application_one_handler).

-export([init/2]).

init(Req, Opts) ->
  { Claims, Req2 } = tools:verifyAuth(Req),
  Method = cowboy_req:method(Req),
  AppId = cowboy_req:binding(appId, Req),
  Result = processRequest(Method, Claims, AppId, Req),
  ReqFinal = cowboy_req:reply(200, tools:resp_headers(),
      Result,
      Req2),
  {ok, ReqFinal, Opts}.

% Internal functions
processRequest(<<"GET">>, _, AppId, _) ->
  AppData = database:getAppDBAppId(binary:bin_to_list(AppId)),
  Result = buildResponse(element(1, list_to_tuple(AppData))),
  jiffy:encode(Result);
processRequest(<<"PUT">>, _, AppId, Req) ->
  {ok, Body, _} = cowboy_req:read_urlencoded_body(Req),
  {_, Name} = lists:keyfind(<<"name">>, 1, Body),
  { _, Description } = lists:keyfind(<<"description">>, 1, Body),
  { _, OwnedTopics } = lists:keyfind(<<"ownedTopics">>, 1, Body),
  { _, SubscribedTopics } = lists:keyfind(<<"subscribedTopics">>, 1, Body),
  { _, Encrypt } = lists:keyfind(<<"encryption">>, 1, Body),
  { _, Filters } = lists:keyfind(<<"filters">>, 1, Body),
  io:format("Filters: ~p~n", [Filters]),
  case Encrypt of
    '\n' ->
        { _, AppData } = database:updateAppDBAppId(binary:bin_to_list(AppId), Name, Description, OwnedTopics, SubscribedTopics, Filters, []),
        Result = buildResponse(element(1, list_to_tuple(AppData))),
        jiffy:encode(Result);
    <<>> ->
      { _, AppData } = database:updateAppDBAppId(binary:bin_to_list(AppId), Name, Description, OwnedTopics, SubscribedTopics, Filters, []),
      Result = buildResponse(element(1, list_to_tuple(AppData))),
      jiffy:encode(Result);
    _ ->
      try encryption:msgEncryption(binary:list_to_bin("Testing"), Encrypt) of
        _ ->
          { _, AppData } = database:updateAppDBAppId(binary:bin_to_list(AppId), Name, Description, OwnedTopics, SubscribedTopics, Filters, binary:bin_to_list(Encrypt)),
          Result = buildResponse(element(1, list_to_tuple(AppData))),
          jiffy:encode(Result)
      catch
        _:_ ->
          jiffy:encode(#{ result => bad })
      end
  end;
processRequest(<<"DELETE">>, _, AppId, _) ->
  database:deleteAppDBAppId(binary:bin_to_list(AppId)),
  jiffy:encode([]).

% %% Internal functions
buildResponse(Data) ->
  {_,Id,Name,Description,ApiKeys,Ownedtopics,SubscribedTopics,CreatedOn,Filters,Encrypt} = element(1, list_to_tuple([Data])),
  io:format("Filters: ~p~n", [Filters]),
  #{
    id => binary:list_to_bin(Id),
    name => binary:list_to_bin(Name),
    description => binary:list_to_bin(Description),
    apiKeys => binary:list_to_bin(ApiKeys),
    ownedTopics => binary:list_to_bin(Ownedtopics),
    subscribedTopics => binary:list_to_bin(SubscribedTopics),
    encrypt => binary:list_to_bin(Encrypt),
    filter => Filters, %binary:list_to_bin(Filters),
    createdOn => binary:list_to_bin(tools:convertDateTime(CreatedOn))
  }.
