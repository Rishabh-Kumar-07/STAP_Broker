%%%-------------------------------------------------------------------
%%% @author rishabh
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Jul 2020 11:09 AM
%%%-------------------------------------------------------------------
-module(simulation).
-author("rishabh").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3, simulation_setup/2, send_msg/2]).

-define(SERVER, ?MODULE).

-record(simulation_state, {}).
-record(perm, {identifier, order}).
-record(client_key_record,{identifier, keys}).
-record(input,{item}).
-record(msg_table,{msg}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
start_link() ->
  PermOpts = [named_table, public, {read_concurrency, true}, {write_concurrency, true}, {keypos, #perm.identifier}],
  _St = ets:new(permutation, [ordered_set | PermOpts ]),
  ClOpts = [named_table, public, {read_concurrency, true}, {write_concurrency, true}, {keypos, #client_key_record.identifier}],
  _Dom = ets:new(client_key, [ordered_set | ClOpts ]),
  COpts = [named_table, public, {read_concurrency, true}, {write_concurrency, true}, {keypos, #input.item}],
  _Do = ets:new(handle_input, [set | COpts ]),
  _D = ets:new(server_key, [ordered_set | ClOpts ]),
  Cpts = [named_table, public, {read_concurrency, true}, {write_concurrency, true}, {keypos, #msg_table.msg}],
  _ = ets:new(input_msg, [set | Cpts ]),
  _ = ets:new(encrypted_msg, [set | Cpts ]),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

simulation_setup(Server, Client) ->
  gen_server:cast(simulation, {setup, Server, Client}).

send_msg(Server, Client) ->
  gen_server:cast(simulation, {send_msg, Server, Client}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #simulation_state{}} | {ok, State :: #simulation_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #simulation_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #simulation_state{}) ->
  {reply, Reply :: term(), NewState :: #simulation_state{}} |
  {reply, Reply :: term(), NewState :: #simulation_state{}, timeout() | hibernate} |
  {noreply, NewState :: #simulation_state{}} |
  {noreply, NewState :: #simulation_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #simulation_state{}} |
  {stop, Reason :: term(), NewState :: #simulation_state{}}).
handle_call(_Request, _From, State = #simulation_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
handle_cast({setup, Server, Client}, _From) ->
  generate_permutation(Server, Client),
  generate_keys(Server, Client),
  State = ok,
  {noreply, State};

handle_cast({send_msg, Server, Client}, _From) ->
  generate_msg(Client);

handle_cast(_Request, State = #simulation_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #simulation_state{}) ->
  {noreply, NewState :: #simulation_state{}} |
  {noreply, NewState :: #simulation_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #simulation_state{}}).
handle_info(_Info, State = #simulation_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #simulation_state{}) -> term()).
terminate(_Reason, _State = #simulation_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #simulation_state{},
    Extra :: term()) ->
  {ok, NewState :: #simulation_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #simulation_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================



generate_permutation(Server, Clients) ->
  L = lists:seq(1, Server),
  _Dont = [store_permutation(Clients, X) || X <- L ].

store_permutation(Clients, Identifier) ->
  Perm = [rand:uniform() || _ <- lists:seq(1, Clients)],
  ets:insert(permutation, #perm{identifier = Identifier, order = Perm}).

generate_keys(Server, Client) ->
  L = lists:seq(1, Client),
  _Dont = [store_key(X,Server) || X <- L],
  Li = ets:tab2list(client_key),
  S = lists:seq(1, Server),
  ets:insert(handle_input, #input{item = Li}),
  [store_key_server(X) || X <- S].

store_key(Identifier, Server) ->
  L = lists:seq(1, Server),
  %%{Server identifier, key, iv, AAD}
  Perm1 = [{X, crypto:strong_rand_bytes(32),crypto:strong_rand_bytes(10),crypto:strong_rand_bytes(10)} || X <- L],
  ets:insert(client_key, #client_key_record{identifier = Identifier, keys = Perm1}).


store_key_server(Server) ->
  L = ets:tab2list(handle_input),
  {_, X} = lists:nth(1,L),
  Server_Key = [lists:nth(Server,R) ||{_,_,R}<-X],
  ets:insert(server_key,#client_key_record{identifier = Server, keys = Server_Key}),
  shuffle_input(Server).

  

shuffle_input(Server) ->
  Order = ets:tab2list(permutation),
  {_, _, Target_Perm} = lists:nth(Server, Order),
  L = ets:tab2list(handle_input),
  {_, Inp} = lists:nth(1,L),
  ets:delete_all_objects(handle_input),
  Temp = lists:zip(Target_Perm, Inp),
  Temp1 = lists:sort(Temp),
  Fin = [ In || { _, In} <- Temp1],
  ets:insert(handle_input, #input{item = Fin}).

generate_msg(Client) ->
  L = lists:seq(1,Client),
  MSG = [{crypto:strong_rand_bytes(32), X}|| X <- L],
  ets:insert(input_msg, #msg_table{msg = MSG}).