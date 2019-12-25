%%%-------------------------------------------------------------------
%% @doc messageMap public API
%% @end
%%%-------------------------------------------------------------------

-module(messagemap_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% Define constant variables
-define(PORT_NUM, os:getenv("MM_PORT")).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    appmanager:start(), % On boot run startup scripts
    Routes = [
     {"/", cowboy_static, {priv_file, messagemap, "index.html" }},
     {"/static/[...]", cowboy_static, {priv_dir, messagemap, "static",
      [{mimetypes, cow_mimetypes, web}]
     }},
     {"/ws", ws_handler, []},
     {"/ws/:id", ws_one_handler, []},
     {"/api/auth", auth_handler, []},
     {"/api/logout", logout_handler, []},
     {"/api/me", me_handler, []},
     {"/api/application", application_handler, []},
     {"/api/application/:appId", application_one_handler, []},
     {"/api/validateEncryption", validate_encryption, []},
     {"/api/topic", topic_handler, []},
     {"/api/topic/:topicId", topic_one_handler, []},
     {"/api/schema", schema_handler, []},
     {"/api/schema/:schemaId", schema_one_handler, []},
     {"/api/stats/:appId", stats_handler, []},
     {"/api/user", user_handler, []},
     {"/api/version", version_handler, []},
     %MESSAGES API ENDPOINTS
     {"/api/auth/token", token_handler, []},
     {"/messages/:version/:topic", messages_handler, []},
     {"/messages/:topic", messages_noversion_handler, []},
     {"/messages", pull_only_messages_handler, []},
     %{"/message/:topic/stats, messagestats_handler, []},
     {"/api/sum", messages_sum_handler, []},
     %% Start Admin API Section
     {"/admin/adduser", admin_add_user_handler, []},
     {"/admin/moduser", admin_modify_user_handler, []}
     %{"/admin/lock", admin_msg_lock_handler, []}
  ],
  Dispatch = cowboy_router:compile([
     {'_', Routes}
  ]),
  {ok, _} = cowboy:start_clear(http, [ { port, erlang:list_to_integer(?PORT_NUM) }], #{
    env => #{dispatch => Dispatch},
    stream_handlers => [cowboy_compress_h, cowboy_stream_h]
  }),
  messagemap_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    appmanager:stop(), % On boot run startup scripts
    ok = cowboy:stop_listener(http).

%%====================================================================
%% Internal functions
%%====================================================================
