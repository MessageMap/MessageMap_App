#!/bin/sh
rm src/*.beam
./rebar3 shell --name $(hostname)
