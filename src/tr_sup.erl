%%%-------------------------------------------------------------------
%%% @author sulongx
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. 9月 2020 7:16 下午
%%%-------------------------------------------------------------------
-module(tr_sup).
-author("sulongx").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor
-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
-spec(init(Args :: term()) ->
    {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
        MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
        [ChildSpec :: supervisor:child_spec()]}}
    | ignore | {error, Reason :: term()}).
init([]) ->
    %% 指明监督者行为
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = #{strategy => one_for_one,
        intensity => MaxRestarts,
        period => MaxSecondsBetweenRestarts},

    %% 指明如何启动和管理子进程
    AChild = #{id => tr_server,
        start => {tr_server, start_link, []},
        restart => permanent,
        shutdown => 2000,
        type => worker,
        modules => [tr_server]},
    %% 返回监督规范
    {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
