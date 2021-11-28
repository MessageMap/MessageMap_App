%%%-------------------------------------------------------------------
%%% @author Benjamin Adams
%%% @copyright (C) 2019, MessageMap.io
%%% @doc
%%%  Administration Modify User to system
%%% @end
%%% Created : 3. Sept 2019
%%%-------------------------------------------------------------------
-module(admin_delete_user_handler).

-export([init/2]).

init(Req, Opts) ->
  { Claims, _ } = tools:verifyAuth(Req),
  IsAdmin = tools:requireAdmin(erlang:is_map(Claims), Claims),
  Method = cowboy_req:method(Req),
  response(IsAdmin, Method, Req, Opts).

% Internal functions
response(<<"Bad">>, _, Req, Opts) ->
  ReqFinal = cowboy_req:reply(200, tools:resp_headers(),
      jiffy:encode(#{ result => <<"Invalid Authorization">> }),
      Req),
  {ok, ReqFinal, Opts};
response(_, Method, Req, Opts) ->
  Result = processRequest(Method, Req),
  ReqFinal = cowboy_req:reply(200, tools:resp_headers(),
      Result,
      Req),
  {ok, ReqFinal, Opts}.

processRequest(<<"DELETE">>, Req) ->
  Username = cowboy_req:binding(userId, Req),
  if
    Username =:= 1 ->
      jiffy:encode(#{ "Error" => "Bad Username"});
    true ->
      database:deleteDBUser(binary_to_list(Username)),
      jiffy:encode(#{ removed => true })
  end.
