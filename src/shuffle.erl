%%%-------------------------------------------------------------------
%%% @author rishabh
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Jul 2020 4:47 PM
%%%-------------------------------------------------------------------
-module(shuffle).
-author("rishabh").

-behaviour(gen_server).

%% API
-export([start_link/0, first_shuffle_setup/2, server_setup/1]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(shuffling_server_state, {}).
-record(setup_initial_table, {identifier, key}).
%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  PubOpts = [named_table, public, {read_concurrency, true}, {write_concurrency, true}, {keypos, #setup_initial_table.identifier}],
  _Topic_status = ets:new(key_setup, [ordered_set | PubOpts ]),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

first_shuffle_setup(Identifier, Keys) ->
  gen_server:cast(shuffle, {fist_setup,Identifier, Keys}).

server_setup(Clients) ->
  gen_server:cast(shuffle,{start_server_setup, Clients}).



%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #shuffling_server_state{}} | {ok, State :: #shuffling_server_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #shuffling_server_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #shuffling_server_state{}) ->
  {reply, Reply :: term(), NewState :: #shuffling_server_state{}} |
  {reply, Reply :: term(), NewState :: #shuffling_server_state{}, timeout() | hibernate} |
  {noreply, NewState :: #shuffling_server_state{}} |
  {noreply, NewState :: #shuffling_server_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #shuffling_server_state{}} |
  {stop, Reason :: term(), NewState :: #shuffling_server_state{}}).
handle_call(_Request, _From, State = #shuffling_server_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages

handle_cast({fist_setup,Identifier, Keys}, _From) ->
  ets:insert(key_setup, #setup_initial_table{identifier = Identifier , key = Keys}),
  Status = ok,
  {noreply, Status};

handle_cast({start_server_setup, Clients}, _From) ->
  generate_keys(),
  generate_permutation(Clients);

handle_cast(_Request, State = #shuffling_server_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #shuffling_server_state{}) ->
  {noreply, NewState :: #shuffling_server_state{}} |
  {noreply, NewState :: #shuffling_server_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #shuffling_server_state{}}).
handle_info(_Info, State = #shuffling_server_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #shuffling_server_state{}) -> term()).
terminate(_Reason, _State = #shuffling_server_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #shuffling_server_state{},
    Extra :: term()) ->
  {ok, NewState :: #shuffling_server_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #shuffling_server_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


generate_keys() ->
  Elgamal_key = os:cmd("python3 elgamal.py").
