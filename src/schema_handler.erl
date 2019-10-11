%%%-------------------------------------------------------------------
%%% @author Benjamin Adams
%%% @copyright (C) 2017, Ethan Solutions
%%% @doc
%%%  Return to the user contents of their token
%%% @end
%%% Created : 8. Sept 2017
%%%-------------------------------------------------------------------
-module(schema_handler).

-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {}).

init(_, Req, _Opts) ->
  {ok, Req, #state{}}.

handle(Req, State=#state{}) ->
  { Claims, Req2 } = tools:verifyAuth(Req),
  % redirect if Claims = Bad
  { Method, _ } = cowboy_req:method(Req),
  Result = processRequest(Method, Claims, Req),
  { ok, ReqFinal } = cowboy_req:reply(200, tools:resp_headers(),
      Result,
      Req2),
  {ok, ReqFinal, State}.

terminate(_Reason, _Req, _State) ->
  ok.

% Internal functions
processRequest(<<"POST">>, _, Req) ->
  {ok, Body, _} = cowboy_req:body_qs(Req),
  {_, Version} = lists:keyfind(<<"version">>, 1, Body),
  {_, Validation} = lists:keyfind(<<"validation">>, 1, Body),
  SchemaData = database:saveSchema(binary:bin_to_list(Version), binary:bin_to_list(Validation)),
  Result = buildResponse(element(1, list_to_tuple(SchemaData))),
  jiffy:encode(Result).

%% Internal functions
buildResponse(Data) ->
  {_,Id,Validation,Version,CreatedOn} = element(1, list_to_tuple([Data])),
  #{
    id => binary:list_to_bin(Id),
    validation => binary:list_to_bin(Validation),
    version => binary:list_to_bin(Version),
    createdOn => binary:list_to_bin(tools:convertDateTime(CreatedOn))
  }.
