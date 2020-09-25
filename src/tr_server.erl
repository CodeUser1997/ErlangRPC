%%%-------------------------------------------------------------------
%%% @author sulongx
%%% @copyright (C) 2020, <SULONGX>
%%% @doc    RPC over TCP server.This module defines a server process that
%%%         listens for incoming TCP connections and allow the user to
%%%         execute RPC commands via that TCP stream.
%%% @end
%%% Created : 24. 9月 2020 5:16 下午
%%%-------------------------------------------------------------------
-module(tr_server).
-behaviour(gen_server).
-author("sulongx").

-include_lib("eunit/include/eunit.hrl").

%% API
-export([
        start_link/1,
        start_link/0,
        get_count/0,
        stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-define(SERVER, ?MODULE). % 将SERVER 设置为模块名
-define(DEFAULT_PORT, 1055). % 定义默认端口号

-record(state, {port, lsock, request_count = 0}). % 保存进程状态

%%% ==================================================================
%%% API
%%% ==================================================================


%%-------------------------------------------------------------------
%% @doc Start the server 派生服务器进程
%%
%% @spec start_link(Port::integer()) -> {ok, Pid}
%% where
%% Pid = pid()
%% @end
%%-------------------------------------------------------------------
start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

%%-------------------------------------------------------------------
%% @doc Call `start_link(Port)` using the default port.
%% @spec start_link() -> {ok, Pid}
%% @end
%%-------------------------------------------------------------------
start_link() ->
    start_link(?DEFAULT_PORT).


%%-------------------------------------------------------------------
%% @doc Fetches the number of requests made to this server.
%% @spec get_count() -> {ok, Count}
%% where
%% Count = integer()
%% @end
%%-------------------------------------------------------------------
get_count() ->
    gen_server:call(?SERVER, get_count). % 调用方会等待应答

%%-------------------------------------------------------------------
%% @doc Stops the server
%% @spec stop() -> ok
%% @end
%%-------------------------------------------------------------------
stop() ->
    gen_server:cast(?SERVER, stop). % 调用方无需坐等应答


%%% ==================================================================
%%% gen_server callbacks
%%% ==================================================================
init([Port]) ->
    {ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
    {ok, #state{port = Port, lsock = LSock}, 0}.

handle_call(get_count, _From, State) ->
    io:format("call发送的同步消息回调~n"),
    {reply, {ok, State#state.request_count}, State}.

handle_cast(stop, State) ->
    io:format("cast发送的异步消息回调~n"),
    {stop, normal, State}.

%% 完成 RPC请求 之后的回调（带外消息回调）
handle_info({tcp, Socket, RawData}, State) ->
    io:format("带外消息回调~n"),
    do_rpc(Socket, RawData),
    RequestCount = State#state.request_count,
    {noreply, State#state{request_count = RequestCount + 1}};
handle_info(timeout, #state{lsock = LSock} = State) ->
    {ok, _Sock} = gen_tcp:accept(LSock),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% ==================================================================
%%% Internal functions
%%% ==================================================================

do_rpc(Socket, RawData) ->
    try
        {M, F, A} = split_out_mfa(RawData),
        Result = apply(M, F, A),
        gen_tcp:send(Socket, io_lib:fwrite("~p~n", [Result]))
    catch
        _Class:Err  ->
            gen_tcp:send(Socket, io_lib:fwrite("~p~n", [Err]))
    end.

split_out_mfa(RawData) ->
    MFA = re:replace(RawData, "\r\n$", "", [{return, list}]),
    {match, [M, F, A]} =
        re:run(MFA,
                "(.*):(.*)\s*\\((.*)\s*\\)\s*.\s*$",
                    [{capture, [1,2,3], list}, ungreedy]),
    {list_to_atom(M), list_to_atom(F), args_to_terms(A)}.

args_to_terms(RawArgs) ->
    {ok, Toks, _Line} = erl_scan:string("[" ++ RawArgs ++ "].", 1),
    {ok, Args} = erl_parse:parse_term(Toks),
    Args.

%% test
start_test() ->
    {ok, _} = tr_server:start_link(1055).
