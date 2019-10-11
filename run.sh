#!/bin/sh
cd /root/msgmap/r_msgmap
export MM_MNESIA_DIR=/var/messageMap
export MM_PORT=8080
export MM_HOSTNAME='development.msgmap.io'
export MM_ENCRYPTION='IYOLoVwM'
mkdir $MM_MNESIA_DIR
#./_build/default/rel/ethandb/bin/ethandb start
tmux new-session -d -s MessageMap ./rebar3 shell --name dev@msgmap --setcookie MessageMap123!
