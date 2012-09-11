#!/bin/sh
rebar compile && erl -boot start_sasl -sname pulsar -pa deps/*/ebin ebin -s pulsar +A 4 +K true -config conf/sys.config
