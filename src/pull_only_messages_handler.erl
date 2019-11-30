%%%-------------------------------------------------------------------
%%% @author Benjamin Adams
%%% @copyright (C) 2019, MessageMap.io
%%% @doc
%%%  Endpoint to just pull messages from the queue
%%% @end
%%% Created : 29. Nov 2019
%%%-------------------------------------------------------------------
-module(pull_only_messages_handler).

-export([init/2]).

init(Req, Opts) ->
  Method = cowboy_req:method(Req),
  AuthToken = cowboy_req:header(<<"authorization">>, Req, []),
  Auth = encryption:ewtDecode(AuthToken),
  #{limit := Limit} = cowboy_req:match_qs([{limit, int, 10}], Req),
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
        Method == <<"GET">> ->
          Result = messages:pull(Auth, Limit),
          Req2 = cowboy_req:reply(200, tools:resp_headers(),
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
