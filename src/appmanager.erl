%%%-------------------------------------------------------------------
%%% @author Ben Adams
%%% @copyright (C) 2017, MessageMap.io
%%% @doc
%%% Module Handle all startup and shutdown actions
%%% @end
%%% Created : 09. Aug 2017
%%%-------------------------------------------------------------------
-module(appmanager).

-export([start/0, stop/0]).

start() ->
  tools:log("info", io_lib:format("Startup Script Has begun", [])),
  database:init(),
  bootup(),
  timer:sleep(1000),
  % Start Backup of apps every minute
  timer:apply_interval(60000, database, backupDB, ["/tmp/db_app_backup.db"]),
  % Run Renew Cert Every 24hrs
  timer:apply_interval(86400, encryption, certRenew, []),
  tools:log("info", io_lib:format("Startup Script Has Finished", [])),
  tools:log("info", io_lib:format("Welcome to MessageMap !!", [])),
  tools:log("info", io_lib:format("~p", [jiffy:decode(tools:version())])).

stop() ->
  tools:log("info", io_lib:format("Shutdown Script Has begun~n", [])),
  % TODO: Find why this halts shutdown
%  mnesia:stop(), % Stopping Database
  tools:log("info", io_lib:format("Shutdown Script Has Finished~n", [])).

%% Internal Functions
bootup() ->
  tools:log("info", "Starting bootup Notes"),
  tools:log("info", "PF Changes"),
  os:cmd("grep -qxF 'block return on ! lo0 proto tcp to port 8080:8080' /etc/pf.conf || echo 'block return on ! lo0 proto tcp to port 8080:8080' >> /etc/pf.conf"),
  os:cmd("grep -qxF 'pass in on egress inet proto tcp from any to port 80 flags S/SA modulate state' /etc/pf.conf || echo 'pass in on egress inet proto tcp from any to port 80 flags S/SA modulate state' >> /etc/pf.conf"),
  os:cmd("rcctl enable pf"),
  os:cmd("rcctl restart pf"),
  os:cmd("pfctl -f /etc/pf.conf"),
  os:cmd("cp ../../../../../sysconfig/httpd.conf /etc/httpd.conf"),
  os:cmd("rcctl -f restart httpd"),
  tools:log("info", "Start to setup ACME"),
  os:cmd("cp ../../../../../sysconfig/acme-client.conf /etc/acme-client.conf"),
  os:cmd("sed -i -e 's/DNS/$(hostname).msgmap.io/g' /etc/acme-client.conf"),
  % TODO: On start configs where not setup Need to retry acme if no: /etc/ssl/cert.fullchain.cert
  os:cmd("acme-client -ADv $(hostname).msgmap.io"),
  timer:sleep(1000),
  tools:log("info", "Starting Nginx configuration"),
  os:cmd("cp ../../../../../sysconfig/nginx.conf /etc/nginx/nginx.conf"),
  os:cmd("rcctl enable nginx"),
  os:cmd("rcctl restart nginx").
