%%%-------------------------------------------------------------------
%%% @author Benjamin Adams
%%% @copyright (C) 2017, Ethan Solutions
%%% @doc
%%%  Return to the user contents of their token
%%% @end
%%% Created : 8. Sept 2017
%%%-------------------------------------------------------------------
-module(schema_handler).

-export([init/2]).

init(Req, Opts) ->
  { Claims, Req2 } = tools:verifyAuth(Req),
  Method = cowboy_req:method(Req),
  response(Claims, Method, Req2, Opts).

% Internal functions
response(<<"Bad">>, _, Req, Opts) ->
  ReqFinal = cowboy_req:reply(200, tools:resp_headers(),
      jiffy:encode(#{ result => <<"Invalid Authorization">> }),
      Req),
  {ok, ReqFinal, Opts};
response(Claims, Method, Req, Opts) ->
  Result = processRequest(Method, Claims, Req),
  { ok, ReqFinal } = cowboy_req:reply(200, tools:resp_headers(),
      Result,
      Req),
  {ok, ReqFinal, Opts}.

% Internal functions
processRequest(<<"POST">>, _, Req) ->
  {ok, Body, _} = cowboy_req:read_urlencoded_body(Req),
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
