%% @private
-module(weberl_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
    Dispatch = [
        {'_', [
            {[<<"ws">>], weberl_ws, []},
            {['...'], cowboy_static, [
                {directory, {priv_dir, weberl, [<<"www">>]}},
                {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
            ]}
        ]}
    ],
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
		{env, [{dispatch, Dispatch}]}
	]),
    io:format(user, "weberl started at http://localhost:8080/index.html~n", []),
	weberl_sup:start_link().

stop(_State) ->
	ok.

