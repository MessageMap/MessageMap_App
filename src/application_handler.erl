%%%-------------------------------------------------------------------
%%% @author Benjamin Adams
%%% @copyright (C) 2017, Ethan Solutions
%%% @doc
%%%  Return to the user contents of their token
%%% @end
%%% Created : 8. Sept 2017
%%%-------------------------------------------------------------------
-module(application_handler).

-export([init/2]).

init(Req, Opts) ->
  { Claims, Req2 } = tools:verifyAuth(Req),
  % redirect if Claims = Bad
  Method = cowboy_req:method(Req),
  Result = processRequest(Method, Claims, Req),
  ReqFinal = cowboy_req:reply(200, tools:resp_headers(),
      Result,
      Req2),
  {ok, ReqFinal, Opts}.

% Internal functions
processRequest(<<"POST">>, _, Req) ->
  {ok, Body, _} = cowboy_req:read_urlencoded_body(Req),
  {_, AppName} = lists:keyfind(<<"appName">>, 1, Body),
  if
    AppName =:= 1 ->
      jiffy:encode(#{ "Error" => "Bad Application Name or Missing Application name"});
    true ->
      AppData = database:saveApp(binary:bin_to_list(AppName), "", [], [], [], []),
      Result = buildResponse(element(1, list_to_tuple(AppData))),
      jiffy:encode(Result)
  end;
processRequest(<<"GET">>, _, _) ->
  AllApps = database:getAllAppDB(),
  Results = buildResponses(AllApps),
  jiffy:encode(Results).

%% Internal functions
buildResponses(Data) ->
  lists:map(fun(N) ->
    buildResponse(N)
  end, Data).

buildResponse(Data) ->
  {_, Id, Name, Description, ApiKeys, Ownedtopics, SubscribedTopics, CreatedOn, Filter, Encrypt} = element(1, list_to_tuple([Data])),
  io:format("Return TODO FIX ON SAVE Filter: ~p~n", [Filter]),
  #{
    id => binary:list_to_bin(Id),
    name => binary:list_to_bin(Name),
    description => binary:list_to_bin(Description),
    apiKeys => binary:list_to_bin(ApiKeys),
    ownedTopics => binary:list_to_bin(Ownedtopics),
    subscribedTopics => binary:list_to_bin(SubscribedTopics),
    encrypt => binary:list_to_bin(Encrypt),
    filters => binary:list_to_bin([Filter]),
    createdOn => binary:list_to_bin(tools:convertDateTime(CreatedOn))
  }.
