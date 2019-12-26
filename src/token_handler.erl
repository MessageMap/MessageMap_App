%%%-------------------------------------------------------------------
%%% @author Benjamin Adams
%%% @copyright (C) 2017, MessageMap.io
%%% @doc
%%%  This file is for oauth
%%% @end
%%% Created : 16. Aug 2017
%%%-------------------------------------------------------------------
-module(token_handler).

-export([init/2]).

-define(access_exp, 5*60). % set to 5 mins
-define(refresh_exp, 14*24*60*60). % set to 14 Days

init(Req, Opts) ->
  Method = cowboy_req:method(Req),
  if
    Method == <<"POST">> ->
      #{ 
         client_id := Client_id,
         grant_type := Grant_type,
         code := Code,
         refresh_token := Refresh_token
       } = cowboy_req:match_qs([
              { client_id, [], <<"none">> },
              { grant_type, [] },
              { code, [], <<"none">> },
              { refresh_token, [], <<"none">> }
              ], Req),
      { StatusCode, Result } = getResponse(Client_id, Grant_type, Code, Refresh_token),
      Req2 = cowboy_req:reply(StatusCode, tools:resp_headers(),
        jiffy:encode(Result),
        Req),
      {ok, Req2, Opts};
    true ->
     Req2 = cowboy_req:reply(405, tools:resp_headers(),
       jiffy:encode(#{ message => <<"Invalid Method">> }),
       Req),
     {ok, Req2, Opts}
  end.

%%%%%%%%%%%%%%%%%%% Internal Functions
%% Build Response
getResponse(Client_id, <<"refresh_token">>, Code, Refresh_token) ->
  io:format("ClientID is not used: ~p~n", [Client_id]),
  io:format("Code is not used: ~p~n", [Code]),
  Claims = encryption:ewtDecode(Refresh_token),
  { _, App } = maps:find(app, Claims),
  returnApp(App);
getResponse(Client_id, <<"authorization_code">>, Code, _) ->
  %Verify Client_id = APP_ID
  App = database:validateAppDBAppIdApiKey(binary:bin_to_list(Client_id), binary:bin_to_list(Code)),
  returnApp(App);
getResponse(_, _, _, _) ->
  { 401,
    #{
      error => <<"Invalid Grant">>
    }
  }.

% Build Return response for the application
returnApp(ResultFromDB) when ResultFromDB =:= #{} ->
  { 401,
    #{
      error => <<"Application Not Found">>
    }
  };
returnApp([]) ->
  { 401,
    #{
      error => <<"Application Not Found">>
    }
  };
returnApp(App) ->
  %TODO: Continue Here for building Ewt (App validation is done)
  { ok, Id } = maps:find(id,App),
  { ok, Owned } = maps:find(ownedTopics,App),
  Schemas = buildSchemas(Owned), %TODO: Fix support no schema
  { ok, Subscribed } = maps:find(subscribedTopics,App),
  %Verify Pub and Sub of App
  Scope = buildScope(Owned, Subscribed),
  Access_Claims = #{
          id => Id,
          scope => Scope,
          info => #{
            topicsOwned => buildIds(Owned),
            schemas => lists:flatten(Schemas),
            topicsSubscribed => buildIds(Subscribed)
          }
  },
  AccessToken = encryption:oauthCreate(Access_Claims, ?access_exp),
  Refresh_Claims = #{
	app => App
  },
  RefreshToken = encryption:oauthCreate(Refresh_Claims, ?refresh_exp),
  { 200,
    #{
      access_token => AccessToken,
      token_type => <<"bearer">>,
      expires_in => ?access_exp,
      refresh_token => RefreshToken,
      scope => Scope
     }
  }.

% Determine the scope allowed for the application
buildScope([],[]) ->
  [];
buildScope(_,[]) ->
  [<<"write">>];
buildScope([],_) ->
  [<<"read">>];
buildScope(_,_) ->
  [<<"read">>, <<"write">>].

% Build response of token Topic Ids
buildIds([]) ->
  false;
buildIds(ListIds) ->
  case lists:member(hd(","),ListIds) of
    true ->
      string:tokens(ListIds, ",");
    _ ->
      [ListIds]
  end.

% Get SchemaId
buildSchemas([]) ->
  [];
buildSchemas(TopicIds) ->
  Topics = case lists:member(hd(","),TopicIds) of
    true ->
      string:tokens(TopicIds, ",");
    _ ->
      [TopicIds]
  end,
  lists:map(fun(Tid) ->
    getSchemas(Tid)
  end, Topics).

getSchemas(TopicId) ->
  [{_,_,_,_,SchemaIds,_}] = database:getTopicDBTopicId(TopicId),
  pullSchemas(SchemaIds).

pullSchemas([]) ->
  [];
pullSchemas(SchemaIds) ->
  SchemaList = case lists:member(hd(","),SchemaIds) of
    true ->
      string:tokens(SchemaIds, ",");
    _ ->
      [SchemaIds]
  end,
  SchemaResult = lists:map(fun(Sid) ->
    [{_,_,Version,_, _}] = database:getSchemaDBSchemaId(Sid),
    #{
      id => Sid,
      v => Version
    }
  end, SchemaList),
  SchemaResult.
