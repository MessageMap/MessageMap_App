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
  tools:logmap("info", #{ "Boot_Start": "Startup Script Has begun" }),
  database:init(),
  bootup(),
  application:start(os_mon),
  timer:sleep(1000),
  database_manager:init(),
  % Start Backup of apps every 10 minutes
  timer:apply_interval(600000, database, backupDB, ["/tmp/db_app_backup.db"]),
  % Run Renew Cert Every 24hrs
  timer:apply_interval(86400, encryption, certRenew, []),
  tools:logmap("info", #{ "Boot_Start": "Startup Script Has Finished" }),
  tools:logmap("info", #{ "Boot_Start": "Welcome to MessageMap !!" }),
  tools:logmap("info", #{ "Boot_Start": #{ "Version": jiffy:decode(tools:version()) } } ).

stop() ->
  tools:logmap("info", #{ "Boot_Shutdown": "Shutdown Script Has begun" }),
  % TODO: Find why this halts shutdown
  mnesia:stop(), % Stopping Database
  application:stop(os_mon),
  tools:logmap("info", #{ "Boot_Shutdown": "Shutdown Script Has Finished" }).

%% Internal Functions
bootup() ->
  tools:logmap("info", #{ "Starting Bootup OS Functions" }),
  os:cmd("grep -qxF 'block return on ! lo0 proto tcp to port 8080:8080' /etc/pf.conf || echo 'block return on ! lo0 proto tcp to port 8080:8080' >> /etc/pf.conf"),
  os:cmd("grep -qxF 'pass in on egress inet proto tcp from any to port 80 flags S/SA modulate state' /etc/pf.conf || echo 'pass in on egress inet proto tcp from any to port 80 flags S/SA modulate state' >> /etc/pf.conf"),
  os:cmd("rcctl enable pf"),
  os:cmd("rcctl restart pf"),
  os:cmd("pfctl -f /etc/pf.conf"),
  os:cmd("cp ../../../../../sysconfig/httpd.conf /etc/httpd.conf"),
  os:cmd("rcctl -f restart httpd"),
  os:cmd("cp ../../../../../sysconfig/acme-client.conf /etc/acme-client.conf"),
  os:cmd("sed -i -e 's/DNS/$(hostname).msgmap.io/g' /etc/acme-client.conf"),
  % TODO: On start configs where not setup Need to retry acme if no: /etc/ssl/cert.fullchain.cert
  os:cmd("acme-client -ADv $(hostname).msgmap.io"),
  timer:sleep(1000),
  os:cmd("cp ../../../../../sysconfig/nginx.conf /etc/nginx/nginx.conf"),
  os:cmd("rcctl enable nginx"),
  os:cmd("rcctl restart nginx").
