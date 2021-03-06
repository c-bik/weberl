-module(weberl_ws).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-record(state, {
        port
    }).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    {ok, Req, #state{}}.

websocket_handle({text, Msg}, Req, #state{port = undefined} = State) ->
    case jsx:decode(Msg) of
        [{<<"start_port">>, Args}] ->
            {match,Args0} =re:run(Args, "(\".*\")|([^ ]+)", [{capture, first, list}, global]),
            ArgsList = lists:append(Args0),
            %io:format(user, "start opts ~p~n", [ArgsList]),
            % ArgsList = lists:append([[binary_to_list(N), binary_to_list(V)] || {N,V} <- Args]),
            Port = erl_port:start_link(ArgsList),
            erlang:start_timer(1000, self(), <<>>),
            io:format(user, "rx start ~p~n", [ArgsList]),
            {reply, {text, <<>>}, Req, State#state{port = Port}};
            %{reply, {text, <<>>}, Req, State};
        Unsupported ->
            io:format(user, "port not running cmd can't be served : ~p~n", [Unsupported]),
            {reply, {text, <<>>}, Req, State}
    end;
websocket_handle({text, Msg}, Req, #state{port = Port} = State) ->
    Message = binary_to_list(Msg),
    io:format(user, "~p:~p RX ~n"++Message, [?MODULE, ?LINE]),
    Port:exec(Message),
    timer:sleep(100),
    Buffer = Port:read_all(),
    Resp = lists:flatten(Buffer),
    RespBin = list_to_binary(Resp),
    io:format(user, "~p:~p TX~n"++Resp, [?MODULE, ?LINE]),
    {reply, {text, RespBin}, Req, State};
    
websocket_handle(_Data, Req, State) ->
    io:format(user, "~p:~p unknown websocket_handle ~p!~n", [?MODULE, ?LINE, _Data]),
    {ok, Req, State}.

websocket_info({timeout, _Ref, _Msg}, Req, #state{port = Port} = State) ->
    Buffer = Port:read_all(),
    Resp = list_to_binary(lists:flatten(Buffer)),
    {reply, {text, << Resp/binary >>}, Req, State};
websocket_info(_Info, Req, State) ->
    io:format(user, "~p:~p unknown websocket_info ~p~n", [?MODULE, ?LINE, _Info]),
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    io:format(user, "~p:~p terminating ~p~n", [?MODULE, ?LINE, _Reason]),
    ok.
