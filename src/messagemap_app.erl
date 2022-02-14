%%%-------------------------------------------------------------------
%%% @author Ben Adams - Ben@MessageMap.IO
%%% @copyright (C) 2017-2022, MessageMap LLC
%% Setup main start up of MessageMap
%% @end
%%%-------------------------------------------------------------------

-module(messagemap_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% Pull value of port from config file
-define(port, erlang:list_to_integer(tools:configFile("port"))).
-define(sslenabled, tools:configFile("ssl_enabled")).
-define(certfile, tools:configFile("ssl_certfile")).
-define(keyfile, tools:configFile("ssl_keyfile")).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    checkDBRestore(),
    % On boot run startup scripts
    appmanager:start(),
    Routes = [
        {"/", cowboy_static, {priv_file, messagemap, "index.html"}},
        {"/static/[...]", cowboy_static,
            {priv_dir, messagemap, "static", [{mimetypes, cow_mimetypes, web}]}},
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
        {"/api/users", user_handler, []},
        {"/api/version", version_handler, []},
        %MESSAGES API ENDPOINTS
        {"/api/auth/token", token_handler, []},
        {"/messages/:version/:topic", messages_handler, []},
        {"/messages/:topic", messages_noversion_handler, []},
        {"/messages", pull_only_messages_handler, []},
        {"/api/sum", messages_sum_handler, []},
        %% Start Admin API Section
        {"/api/adduser", admin_add_user_handler, []},
        {"/api/moduser", admin_modify_user_handler, []},
        {"/api/deluser/:userId", admin_delete_user_handler, []}
    ],
    Dispatch = cowboy_router:compile([
        {'_', Routes}
    ]),
    starter(?sslenabled, Dispatch),
    messagemap_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    % On boot run startup scripts
    appmanager:stop(),
    ok = cowboy:stop_listener(https).

%%====================================================================
%% Internal functions
%%====================================================================
% TODO: Fix this for enabling ssl
starter("True", Dispatch) ->
    {ok, _} = cowboy:start_tls(
        https,
        [
            {port, ?port},
            {cacertfile, "/etc/ssl/certs/ca-certificates.crt"},
            {certfile, ?certfile},
            {keyfile, ?keyfile}
        ],
        #{
            env => #{dispatch => Dispatch}
        }
    );
starter(_, Dispatch) ->
    {ok, _} = cowboy:start_clear(http, [{port, ?port}], #{
        env => #{dispatch => Dispatch}
    }).

checkDBRestore() ->
    try tools:configFile("database_restore_filename") of
        _ ->
            Filename = tools:configFile("database_restore_filename"),
            tools:log("info", "Boot_Start- Starting Database Restore from File"),
            tools:log("info", io_lib:format("Boot_Start - Restore Filename: ~s", [Filename]))
    catch
        _:_ ->
            tools:log("info", "Boot_Start - No Database Restore")
    end.
