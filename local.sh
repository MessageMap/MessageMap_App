#!/bin/sh
rm src/*.beam
tmux new-session -d -s MessageMap ./rebar3 shell --name $(hostname)