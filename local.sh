#!/bin/sh
export MM_MNESIA_DIR=/var/messageMap
export MM_PORT=8080
export MM_HOSTNAME='laptop.msgmap.io'
export MM_ENCRYPTION='IYOLoVwM'
./rebar3 shell --name laptop@msgmap --setcookie MessageMap123!