#!/bin/sh
erl -pa ebin/ -pa deps/*/ebin -eval "apply(weberl, start, [])"
