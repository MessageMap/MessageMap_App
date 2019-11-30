%%%-------------------------------------------------------------------
%%% @author Benjamin Adams
%%% @copyright (C) 2017, MessageMap.io
%%% @doc
%%%  This Intercepts Requests for Handeling messages
%%% @end
%%% Created : 16. Aug 2017
%%%-------------------------------------------------------------------
-module(messagestats_handler).

-export([init/2]).

init(Req, Opts) ->
  Method = cowboy_req:method(Req),
  %{ Version, _} = cowboy_req:binding(version, Req), TODO: Add to e used with stats
  Topic = cowboy_req:binding(topic, Req),
  AuthToken = cowboy_req:header(<<"authorization">>, Req, []),
  Auth = encryption:ewtDecode(AuthToken),
  if
    AuthToken == [] ->
       {ok, Req2} = cowboy_req:reply(401, tools:resp_headers(),
         jiffy:encode(#{ message => <<"Invalid Authorization">> }),
         Req),
       {ok, Req2, Opts};
    fail == Auth ->
       {ok, Req2} = cowboy_req:reply(401, tools:resp_headers(),
         jiffy:encode(#{ message => <<"Invalid Authorization">> }),
         Req),
       {ok, Req2, Opts};
    true ->
      if
        Method == <<"GET">> ->
          Result = messages:stats(string:toLower(Topic)),
          {ok, Req2} = cowboy_req:reply(200, tools:resp_headers(),
            jiffy:encode(Result),
            Req),
          {ok, Req2, Opts}
       end
  end.
