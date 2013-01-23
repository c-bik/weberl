-module(erl_port).
-behaviour(gen_server).

-record(state, {
    port,
    buffer = empty,
    owner
}).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

%% API
-export([start_link/0
        ,exec/2
        ,readline/1
        ,read_all/1]).

exec(Message, {?MODULE, Pid}) -> gen_server:call(Pid, {send, Message}).
readline({?MODULE, Pid})      ->
    case gen_server:call(Pid, {get_line}) of
    empty ->
        timer:sleep(100),
        readline({?MODULE, Pid});
    Res ->
        Res
    end.

read_all({?MODULE, Pid}) -> read_all([], Pid).
read_all(Buffer, Pid) ->
    case gen_server:call(Pid, {get_line}) of
    empty ->
        timer:sleep(100),
        read_all(Buffer, Pid);
    finished -> Buffer;
    Res ->
        read_all(Buffer ++ [Res], Pid)
    end.

start_link() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [self()], []),
    {?MODULE, Pid}.

init([OwnerPid]) when is_pid(OwnerPid) ->
    {ok, Executable} = application:get_env(weberl, exec),
    case (catch erlang:open_port({spawn_executable, os:find_executable(Executable)}, [stream, exit_status, use_stdio])) of
        {'EXIT', Reason} ->
            io:format(user, "~p could not open port: ~p~n", [?MODULE, Reason]),
            {stop, Reason};
        Port ->
            io:format(user, "~p:~p init opened new port(~p): ~p~n", [?MODULE, self(), Port, erlang:port_info(Port)]),
            {ok, #state{port=Port, owner=OwnerPid}}
    end.

handle_call({send, Message}, _From, #state{port=Port} = State) ->
    true = erlang:port_command(Port, Message),
%    io:format(user, "~p:~p TX ~p~n", [?MODULE, self(), Message]),
    {reply, ok, State};
handle_call({get_line}, _From, #state{buffer = Buffer} = State) ->
    case Buffer of
    empty ->
        {reply, empty, State};
    [] ->
        {reply, finished, State#state{buffer = empty}};
    [L|Rest] ->
%        io:format(user, "~p:~p read ~p~n", [?MODULE, self(), L]),
        {reply, L, State#state{buffer = Rest}}
    end;
handle_call(Req, From, State) ->
    io:format(user, "~p:~p unknown handle_call ~p:~p~n", [?MODULE, self(), From, Req]),
    {reply, ok, State}.

handle_info({Port, {data, Data}}, #state{port=Port, buffer = Buffer} = State) ->
%    io:format(user, "~p:~p RX ~p~n", [?MODULE, self(), Data]),
    case Buffer of
    empty  -> {noreply, State#state{buffer = [Data]}};
    Buffer -> {noreply, State#state{buffer = Buffer ++ [Data]}}
    end;
handle_info({Port, {exit_status, Status}}, #state{port = Port} = State) ->
    io:format(user, "~p:~p exited with status ~p~n", [?MODULE, self(), Status]),
    case Status of
        0 ->
            {stop, normal, State};
        Other ->
            {stop, {port_exit, Other}, State}
    end;
handle_info(Data, State) ->
    io:format(user, "~p:~p unknown handle_info ~p~n", [?MODULE, self(), Data]),
    {noreply, State}.

handle_cast(Data, State) ->
    io:format(user, "~p:~p unknown handle_cast ~p~n", [?MODULE, self(), Data]),
    {noreply, State}.

terminate(Reason, #state{port=Port, owner=OwnerPid}) ->
    io:format(user, "~p:~p terminating ~p~n", [?MODULE, self(), Reason]),
    catch erlang:port_close(Port),
    exit(OwnerPid,kill),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

% EUnit tests --

-include_lib("eunit/include/eunit.hrl").
-define(Table, test_table_123).

setup() ->
    io:format(user, "+===========================================================+~n",[]),
    erl_port:start_link().

teardown(_Pid) ->
    io:format(user, "+===========================================================+~n",[]).

db_test_() ->
    {timeout, 1000000, {
        setup,
        fun setup/0,
        fun teardown/1,
        {with, [
                fun basic_test/1
        ]}
        }
    }.

basic_test(Port) ->
    io:format(user, "----------------------- basic_test -------------------------~n",[]),
    Port:exec("help().\n"),
    Buffer = Port:read_all(),
    io:format(user, "~n>>>>>>>~n" ++ lists:flatten([" " ++ D || D <- Buffer]) ++ "~n>>>>>>>~n", []),
    io:format(user, "------------------------------------------------------------~n",[]).
