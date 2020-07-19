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

-include("rec.hrl").
%%%-include("emqx.hrl").
%%% -include("logger.hrl").
%%%-include("types.hrl").
%%%-include("emqx_mqtt.hrl").

%% API
-export([start_link/4]).
-export([create_tabs/0]).

%% PubSub
%%-export([ subscribe/1
%%  , subscribe/2
%%  , subscribe/3
%%]).

%%-export([unsubscribe/1]).

%%-export([subscriber_down/1]).

%%-export([ publish/1
%%  , safe_publish/1
%%]).

%%-export([dispatch/2]).

%% PubSub Infos
%%-export([ subscriptions/1
%%  , subscribers/1
%%  , subscribed/2
%%]).

%%-export([ get_subopts/2
%%  , set_subopts/2
%%]).

%%-export([topics/0]).

%% Stats fun
%%-export([stats_fun/0]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-import(emqx_tables, [lookup_value/2, lookup_value/3]).

-ifdef(TEST).
-compile(export_all).
-compile(nowarn_export_all).
-endif.

-define(BROKER, ?MODULE).

%% ETS tables for PubSub
-define(REGUSER, erl_reguser).
-define(TOPICKEY, erl_topickey).
-define(SUBSCRIBERS, erl_subscribers).

%% Guards
-define(is_subid(Id), (is_binary(Id) orelse is_atom(Id))).



-define(SERVER, ?MODULE).

-record(broker_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)


-spec(create_tabs() -> ok).
create_tabs() ->
  TabOpts = [public, {read_concurrency, true}, {write_concurrency, true}],

  %% Register User: {Username, Key} -> Reguser
  ok = emqx_tables:new(?REGUSER, [set | TabOpts]),

  %% Topic Key: {Topicname, key} -> TopicKey
  %% duplicate_bag: o(1) insert
  ok = emqx_tables:new(?TOPICKEY, [set | TabOpts]).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #broker_state{}} | {ok, State :: #broker_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #broker_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #broker_state{}) ->
  {reply, Reply :: term(), NewState :: #broker_state{}} |
  {reply, Reply :: term(), NewState :: #broker_state{}, timeout() | hibernate} |
  {noreply, NewState :: #broker_state{}} |
  {noreply, NewState :: #broker_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #broker_state{}} |
  {stop, Reason :: term(), NewState :: #broker_state{}}).
handle_call(_Request, _From, State = #broker_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #broker_state{}) ->
  {noreply, NewState :: #broker_state{}} |
  {noreply, NewState :: #broker_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #broker_state{}}).
handle_cast(_Request, State = #broker_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #broker_state{}) ->
  {noreply, NewState :: #broker_state{}} |
  {noreply, NewState :: #broker_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #broker_state{}}).
handle_info(_Info, State = #broker_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #broker_state{}) -> term()).
terminate(_Reason, _State = #broker_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #broker_state{},
    Extra :: term()) ->
  {ok, NewState :: #broker_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #broker_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


start_link(_Arg0, _Arg1, _Arg2, _Arg3) ->
  erlang:error(not_implemented).