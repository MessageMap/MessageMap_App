%%%-------------------------------------------------------------------
%%% @author Benjamin Adams
%%% @copyright (C) 2020, MessageMap.io
%%% @doc
%%%  This Intercepts Requests for Handeling messages
%%% @end
%%% Created : 04. Jun 2020
%%%-------------------------------------------------------------------
-module(messages_noversion_handler).

-export([init/2]).

init(Req, Opts) ->
  Method = cowboy_req:method(Req),
  Topic = cowboy_req:binding(topic, Req),
  FullAuthToken = cowboy_req:header(<<"authorization">>, Req, <<"Bad">>),
  AuthToken = lists:last(string:tokens(binary:bin_to_list(FullAuthToken), " ")),
  Auth = encryption:ewtDecode(binary:list_to_bin(AuthToken)),
  RequestTime = cowboy_req:header(<<"x-request-time">>, Req, binary:list_to_bin(uuid:to_string(uuid:uuid4()))),
  { OsOk, _ } = tools:osStats(),
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
        Method == <<"POST">> andalso OsOk ->
          {ok, [{ Payload, _}] , _} = cowboy_req:read_urlencoded_body(Req),
          Ltopic = binary:bin_to_list(Topic),
          { Status, Result } = processTopic(database:getTopicDB(Ltopic), Topic, Auth, Payload, RequestTime),
          Req2 = cowboy_req:reply(Status, tools:resp_headers(),
            jiffy:encode(Result),
            Req),
          {ok, Req2, Opts};
        OsOk =/= true ->
          Req2 = cowboy_req:reply(429, tools:resp_headers(),
            jiffy:encode(#{ message => <<"Environment Overloaded">> }),
            Req),
          {ok, Req2, Opts};
        true ->
         Req2 = cowboy_req:reply(405, tools:resp_headers(),
           jiffy:encode(#{ message => <<"Invalid Method">> }),
           Req),
         {ok, Req2, Opts}
       end
  end.

% Internal functions
sendMessages([], Topic, Auth, Payload, RequestTime) ->
  messages:push("None", Topic, Auth, Payload, RequestTime);
sendMessages(_,_,_,_,_) ->
  { 422, #{ message => <<"Topic Requires a Schema Version">> } }.

processTopic([{topics, _, _, _, CheckSchemas, _}], Topic, Auth, Payload, RequestTime) ->
  sendMessages(CheckSchemas, Topic, Auth, Payload, RequestTime);
processTopic(_, _, _, _, _) ->
  { 404, [] }.
