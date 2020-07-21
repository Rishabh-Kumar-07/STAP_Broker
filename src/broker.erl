%%%-------------------------------------------------------------------
%%% @author rishabh
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Jul 2020 8:49 PM
%%%-------------------------------------------------------------------
-module(broker).
-author("rishabh").
-behaviour(gen_server).

%% Record file for database.
-include("rec.hrl").

%%%-include("emqx.hrl").
%%% -include("logger.hrl").
%%%-include("types.hrl").
%%%-include("emqx_mqtt.hrl").

%% API
-export([start/0, stop/0, subscribe/3, create_topic/3, publish/2]).
-export([init/1, handle_cast/2, terminate/2, handle_info/2, handle_call/3]).


%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)

subscribe(Topic, Username, Enc_Add) ->
  gen_server:call(broker, {subscribe, Topic, Username, Enc_Add}).

create_topic(Publisher_Id, Topic_name, Topic_key) ->
  gen_server:call(broker, {create_topic, Publisher_Id, Topic_name, Topic_key}).

publish(Topic_name, Data) ->
  gen_server:cast(broker, {publish, Topic_name, Data}).










%%------------------------------------------------------------------------------
%% Subscribe API
%%------------------------------------------------------------------------------
%%-spec(subscribe(Enc_addr, Node_flag, Username) -> TopicKey).
%%subscribe() ->
%%  _Reply = gen_server:call(broker_server, {subscribe}).
  %%Sub = #subscribers{username=Username, enc_address=Enc_addr, node_flag=Node_flag},
  %%%%ets:insert(?SUBSCRIBERS, #subscribers{username = Username, enc_address = Enc_addr, node_flag = Node_flag}).

%%allocate() ->
%%  gen_server:call(broker_server, {allocate, self()}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


%% @private
%% @doc Handling call messages
%%-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
%%    State :: #broker_state{}) ->
%%  {reply, Reply :: term(), NewState :: #broker_state{}} |
%%  {reply, Reply :: term(), NewState :: #broker_state{}, timeout() | hibernate} |
%%  {noreply, NewState :: #broker_state{}} |
%%  {noreply, NewState :: #broker_state{}, timeout() | hibernate} |
%%  {stop, Reason :: term(), Reply :: term(), NewState :: #broker_state{}} |
%%  {stop, Reason :: term(), NewState :: #broker_state{}}).

handle_call({create_topic, Publisher_Id, Topic_name, Topic_key}, _From, _State) ->
  PubOpts = [named_table, public, {read_concurrency, true}, {write_concurrency, true}, {keypos, #subscribers.enc_address}],
  Status = all_ok,
  Topic_Status = ets:new(Topic_name, [set | PubOpts ]),
  ets:insert(topic_key_table, #topic_key{topic_name = Topic_name, key = Topic_key}),
  {reply, Topic_Status, Status};


handle_call({subscribe, Topic, Username, Enc_Add}, _From, _State) ->
  Status = all_ok,
  Insert_sub = insert_sub(Topic, Username, Enc_Add),
  {reply, Insert_sub, Status}.

handle_cast({publish, Topic_name, Data}, _From) ->
  Event_start = latency_start,
  Time_start = erlang:timestamp(),
  ets:insert(topic_key_table, #latency{event = Event_start, timestamp = Time_start}),
  List_sub = ets:tab2list(Topic_name),
  ok = send_msg(List_sub, Data),
  Event_end = latency_end,
  Time_end = erlang:timestamp(),
  ets:insert(topic_key_table, #latency{event = Event_end, timestamp = Time_end}),
  {noreply, Data};

handle_cast(stop, LoopData) ->
  {stop, normal, LoopData}.

handle_info(_Msg, LoopData) ->
  {noreply, LoopData}.


terminate(_Reason, _LoopData) ->
  ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================


start() ->
  PubOpts = [named_table, public, {read_concurrency, true}, {write_concurrency, true}, {keypos, #topic_key.topic_name}],
  _Topic_status = ets:new(topic_key_table, [set | PubOpts ]),
  ExpOpts = [named_table, public, {read_concurrency, true}, {write_concurrency, true}, {keypos, #latency.event}],
  _Ev_st = ets:new(latency_exp, [set | ExpOpts]),
  gen_server:start_link({local, broker}, broker, [], []).

init(_Args) ->
   _Status = hii,
  {ok, _Status}.

stop() ->
  gen_server:cast(broker, stop).

insert_sub(Topic, Username, Enc_add) ->
  ets:insert(Topic, #subscribers{username = Username, enc_address = Enc_add, node_flag = true}),
  Reply = search_topickey(Topic),
  {Reply}.

search_topickey(Topic) ->
  Table = topic_key_table,
  ets:lookup_element(Table, Topic, 3).

send_msg([H|T], Data) ->
  tcp_client(H, Data),
  _ = send_msg(T, Data);

send_msg([], Data) ->
  _ =  ok.

tcp_client(Add, Data) ->
  {_Sub, _Username, Address, _Node} = Add,
  {Ip, Port} = Address,
  io:fwrite("~p~n", [Ip]),
  io:fwrite("~p~n", [Port]),
  {ok, Sock} = gen_tcp:connect(Ip, Port, [binary, {packet, 0}]),
  ok = gen_tcp:send(Sock, Data),
  ok = gen_tcp:close(Sock).
  %%Address.



