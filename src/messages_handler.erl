%%%-------------------------------------------------------------------
%%% @author Benjamin Adams
%%% @copyright (C) 2020, MessageMap.io
%%% @doc
%%%  This Intercepts Requests for Handeling messages
%%% @end
%%% Created : 04. Jun 2020
%%%-------------------------------------------------------------------
-module(messages_handler).

-export([init/2]).

init(Req, Opts) ->
  Method = cowboy_req:method(Req),
  Version = cowboy_req:binding(version, Req),
  Topic = cowboy_req:binding(topic, Req),
  FullAuthToken = cowboy_req:header(<<"authorization">>, Req, <<"Bad">>),
  AuthToken = lists:last(string:tokens(binary:bin_to_list(FullAuthToken), " ")),
  Auth = encryption:ewtDecode(binary:list_to_bin(AuthToken)),
  RequestTime = cowboy_req:header(<<"x-request-time">>, Req, binary:list_to_bin(uuid:to_string(uuid:uuid4()))),
  if
    AuthToken == [] ->
       Req2 = cowboy_req:reply(401, tools:resp_headers(),
         jiffy:encode(#{ message => <<"Invalid Authorization">> }),
         Req),
       {ok, Req2, Opts};
    fail == Auth ->
       Req2 = cowboy_req:reply(401, tools:resp_headers(),
         jiffy:encode(#{ message => <<"Invalid Authorization">> }),
         Req),
       {ok, Req2, Opts};
    true ->
      if
        Method == <<"POST">> ->
          {ok,  [{ Payload, _}] , _} = cowboy_req:read_urlencoded_body(Req),
          { Status, Result } = messages:push(Version, Topic, Auth, Payload, RequestTime),
          Req2 = cowboy_req:reply(Status, tools:resp_headers(),
            jiffy:encode(Result),
            Req),
          {ok, Req2, Opts};
        true ->
         Req2 = cowboy_req:reply(405, tools:resp_headers(),
           jiffy:encode(#{ message => <<"Invalid Method">> }),
           Req),
         {ok, Req2, Opts}
       end
  end.
