%%%-------------------------------------------------------------------
%%% @author rishabh
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Jul 2020 4:52 PM
%%%-------------------------------------------------------------------
-module(performance).
-author("rishabh").

-include("rec.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([add_subscriber/1]).

add_subscriber(Topic) ->
  Ports = lists:seq(10000,20000),
  add_sub(Ports, Topic).

add_sub([H|T], Topic) ->
  insert_sub(Topic, H, H),
  _ = add_sub(T, Topic);

add_sub([], _Data) ->
  _ =  ok.

simple_test() ->
  ?assert(true).

insert_sub(Topic, Username, Addr) ->
  Enc_add = {{127,0,0,1}, Addr},
  ets:insert(Topic, #subscribers{username = Username, enc_address = Enc_add, node_flag = true}).