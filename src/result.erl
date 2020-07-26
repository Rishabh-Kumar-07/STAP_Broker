%%%-------------------------------------------------------------------
%%% @author rishabh
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Jul 2020 8:25 PM
%%%-------------------------------------------------------------------
-module(result).
-author("rishabh").

-include_lib("eunit/include/eunit.hrl").

-export([create_table/0, perform_test/0]).

-record(eval,{event,timestamp_start,timestamp_stop}).

create_table() ->
  KeyOpts = [named_table, public, {read_concurrency, true}, {write_concurrency, true}, {keypos, #eval.event}],
  ets:new(shuffle, [set | KeyOpts ]),
  ets:new(setup, [set | KeyOpts ]),
  ets:new(rpc, [set | KeyOpts ]).

perform_test() ->
  Time_start_shuffle = erlang:timestamp(),
  simulation:simulation_setup(100, 1000),
  Time_stop_shuffle = erlang:timestamp(),
  ets:insert(shuffle,#eval{event = shuffle, timestamp_start = Time_start_shuffle,timestamp_stop = Time_stop_shuffle}),
  Time_start_setup = erlang:timestamp(),
  simulation:send_msg(100, 1000),
  Time_stop_setup = erlang:timestamp(),
  ets:insert(setup,#eval{event = setup, timestamp_start = Time_start_setup,timestamp_stop = Time_stop_setup}),
  Time_start_rpc = erlang:timestamp(),
  Add = {{127,0,0,1},8888},
  Data = "testmsg",
  tcp_client(Add,Data),
  Time_stop_rpc = erlang:timestamp(),
  ets:insert(shuffle,#eval{event = rpc, timestamp_start = Time_start_rpc,timestamp_stop = Time_stop_rpc}).

tcp_client(Add, Data) ->
  {Ip, Port} = Add,
  io:fwrite("~p~n", [Ip]),
  io:fwrite("~p~n", [Port]),
  {ok, Sock} = gen_tcp:connect(Ip, Port, [binary, {packet, 0}]),
  ok = gen_tcp:send(Sock, Data),
  ok = gen_tcp:close(Sock).

simple_test() ->
  ?assert(true).
