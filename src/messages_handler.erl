%%%-------------------------------------------------------------------
%%% @author Benjamin Adams
%%% @copyright (C) 2017, MessageMap.io
%%% @doc
%%%  This Intercepts Requests for Handeling messages
%%% @end
%%% Created : 16. Aug 2017
%%%-------------------------------------------------------------------
-module(messages_handler).

-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {}).

init(_, Req, _Opts) ->
  {ok, Req, #state{}}.

handle(Req, State=#state{}) ->
  { Method, _ } = cowboy_req:method(Req),
  { Version, _} = cowboy_req:binding(version, Req),
  { Topic, _} = cowboy_req:binding(topic, Req),
  { AuthToken, _ } = cowboy_req:header(<<"authorization">>, Req, []),
  Auth = encryption:ewtDecode(AuthToken),
  if
    AuthToken == [] ->
       {ok, Req2} = cowboy_req:reply(401, tools:resp_headers(),
         jiffy:encode(#{ message => <<"Invalid Authorization">> }),
         Req),
       {ok, Req2, State};
    fail == Auth ->
       {ok, Req2} = cowboy_req:reply(401, tools:resp_headers(),
         jiffy:encode(#{ message => <<"Invalid Authorization">> }),
         Req),
       {ok, Req2, State};
    true ->
      if
        Method == <<"GET">> ->
          QsVals = cowboy_req:parse_qs(Req),
          {_, Limit} = lists:keyfind(<<"limit">>, 10, QsVals),
          Result = messages:pull(Version, Topic, Auth, Limit),
          {ok, Req2} = cowboy_req:reply(200, tools:resp_headers(),
            jiffy:encode(Result),
            Req),
          {ok, Req2, State};
        Method == <<"POST">> ->
          {ok,  [{ Payload, _}] , _} = cowboy_req:body_qs(Req),
          Result = messages:push(Version, Topic, Auth, Payload),
          {ok, Req2} = cowboy_req:reply(200, tools:resp_headers(),
            jiffy:encode(Result),
            Req),
          {ok, Req2, State};
        true ->
         {ok, Req2} = cowboy_req:reply(405, tools:resp_headers(),
           jiffy:encode(#{ message => <<"Invalid Method">> }),
           Req),
         {ok, Req2, State}
       end
  end.

terminate(_Reason, _Req, _State) ->
  ok.
