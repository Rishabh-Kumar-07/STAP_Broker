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
-export([start_link/0, first_shuffle_setup/2, server_setup/1, get_pubkey/0, client_shuffle/0, aes_key/2, aes_msg/2]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(shuffling_server_state, {}).
-record(setup_initial_table, {identifier, key}).
-record(elg_keys, {modulus, generator, pub_key, pri_key}).
-record(perm, {order}).
-record(dec, {input, keys}).
-record(aes_key_rec,{identifier, key}).
%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  PubOpts = [named_table, public, {read_concurrency, true}, {write_concurrency, true}, {keypos, #setup_initial_table.identifier}],
  _Topic_status = ets:new(key_setup, [ordered_set | PubOpts ]),
  KeyOpts = [named_table, public, {read_concurrency, true}, {write_concurrency, true}, {keypos, #elg_keys.pub_key}],
  _Stat = ets:new(elgamal, [set | KeyOpts ]),
  PermOpts = [named_table, public, {read_concurrency, true}, {write_concurrency, true}, {keypos, #perm.order}],
  _St = ets:new(permutation, [set | PermOpts ]),
  DecOpts = [named_table, public, {read_concurrency, true}, {write_concurrency, true}, {keypos, #dec.keys}],
  _STA = ets:new(decproof, [set | DecOpts ]),
  AesOpts = [named_table, public, {read_concurrency, true}, {write_concurrency, true}, {keypos, #aes_key_rec.identifier}],
  _AES = ets:new(aes_key_tab, [ordered_set | AesOpts ]),
  _MSG = ets:new(aes_msg_tab, [ordered_set | AesOpts ]),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

first_shuffle_setup(Identifier, Keys) ->
  gen_server:cast(shuffle, {fist_setup,Identifier, Keys}).

client_shuffle() ->
  gen_server:cast(shuffle,{client_shuffle}).

server_setup(Clients) ->
  gen_server:cast(shuffle,{start_server_setup, Clients}).

get_pubkey() ->
  gen_server:call(shuffle,{pub_key}).

aes_key(Identifier, Key) ->
  gen_server:cast(shuffle,{aes_key, Identifier, Key}).

aes_msg(Identifier, Msg) ->
  gen_server:cast(shuffle,{aes_msg, Identifier, Msg}).

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

handle_call(_Request, _From, State = #shuffling_server_state{}) ->
  {reply, ok, State};

handle_call({pub_key}, _Req, _From) ->
  Pub_Key = fetch_pubkey(),
  State = ok,
  {reply, Pub_Key, State}.

%% @private
%% @doc Handling cast messages

handle_cast({fist_setup,Identifier, Keys}, _From) ->
  ets:insert(key_setup, #setup_initial_table{identifier = Identifier , key = Keys}),
  Status = ok,
  {noreply, Status};

handle_cast({start_server_setup, Clients}, _From) ->
  generate_keys(),
  State = ok,
  generate_permutation(Clients),
 {noreply, State};

handle_cast({client_shuffle}, _From) ->
  Shuffled = shuffle_input(),
  State = ok,
  _Dec_and_proof = dec_and_proof(Shuffled),
  {noreply, State};

handle_cast({aes_key, Identifier, Key}, _From ) ->
  ets:insert(aes_key_tab, #aes_key_rec{identifier = Identifier, key = Key}),
  State = ok,
  {noreply, State};


handle_cast({aes_msg, Identifier, Key}, _From ) ->
  ets:insert(aes_msg_tab, #aes_key_rec{identifier = Identifier, key = Key}),
  State = ok,
  {noreply, State};

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
  Keys = os:cmd("python3 elgamal.py"),
  Var = [list_to_integer(I) || I <- string:tokens(Keys,", ")],
  [Mod, Gen, Pub, Pri] = Var,
  ets:insert(elgamal, #elg_keys{modulus = Mod, generator = Gen, pub_key = Pub, pri_key = Pri}).

generate_permutation(Clients) ->
  Perm = [rand:uniform() || _ <- lists:seq(1, Clients)],
  ets:insert(permutation, #perm{order = Perm}).

fetch_pubkey() ->
  [Key] = ets:tab2list(elgamal),
  {_Sub ,Mod, Gen, Pub, Pri} = Key,
  Reply = {Mod, Gen, Pub},
  Reply.

shuffle_input() ->
  Inp = ets:tab2list(key_setup),
  [Order] = ets:tab2list(permutation),
  {_Na, Perm}  = Order,
  Inp_Cli = lists:zip(Perm, Inp),
  Reply = lists:sort(Inp_Cli),
  Reply.

dec_and_proof(Shuffled) ->
  Input = [X||{_,X} <- Shuffled],
  Dec = [X||{_,_ ,X} <- Input],
  Keys = get_key(),
  ets:insert(decproof, #dec{input = Dec, keys = Keys}),
  [Data] = ets:tab2list(decproof),
  {_Dnt, ClIp, Key} = Data,
  file:write_file("client_input", io_lib:fwrite("~p.\n", [ClIp])),
  file:write_file("keys", io_lib:fwrite("~p.\n", [Key])),
  %%file:write_file("decrypt", io_lib:fwrite("~p.\n", [Data])),
  ets:delete_all_objects(decproof),
  _ = os:cmd("python3 dec_and_proof.py").

get_key() ->
  [Key] = ets:tab2list(elgamal),
  {_Sub ,Mod, Gen, Pub, Pri} = Key,
  Reply = {Mod, Gen, Pub, Pri},
  Reply.