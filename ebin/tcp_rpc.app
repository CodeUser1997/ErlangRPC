%%%-------------------------------------------------------------------
%%% @author sulongx
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. 9月 2020 4:23 下午
%%%-------------------------------------------------------------------
{application, tcp_rpc, [
    {description, "RPC server for Erlang and OTP in action"},
    {vsn, "0.1.0"},
    {modules,[
        tr_app,
        tr_sup,
        tr_server
    ]},
    {registered, [
        tr_sup, tr_server
    ]},
    {applications, [
        kernel,
        stdlib
    ]},
    {mod, {tr_app, []}},
    {env, []}
]}.
