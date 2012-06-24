#!/bin/sh
rebar compile && erl -boot start_sasl -sname graph_rhythm -pa deps/*/ebin ebin -s graph_rhythm +A 4
