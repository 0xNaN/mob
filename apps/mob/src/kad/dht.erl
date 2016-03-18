-module(dht).

-behaviour(gen_server).

%% API
-export([start/1]).
-export([get/3]).
-export([set/3]).
-export([join/3]).

%% gen_server callbacks
-export([init/1,
     handle_call/3,
     handle_cast/2,
     handle_info/2,
     terminate/2,
     code_change/3]).

-record(state, {alpha}).

start(Alpha) ->
    gen_server:start(?MODULE, [Alpha], []).

get(DhtPid, Peer, Key) ->
    gen_server:call(DhtPid, {get, Peer, Key}).

set(DhtPid, Peer, Pair) ->
    gen_server:call(DhtPid, {set, Peer, Pair}).

join(DhtPid, Peer, BootstrapPeer) ->
    gen_server:call(DhtPid, {join, Peer, BootstrapPeer}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Alpha]) ->
    {ok, #state{alpha = Alpha}}.

handle_call({get, {PeerPid, _}, Key}, _From, State) ->
    Reply = gen_server:call(PeerPid, {iterative_find_value, Key}),
    {reply, Reply, State};

handle_call({set, {PeerPid, _}, Pair}, _From, State) ->
    Reply = gen_server:call(PeerPid, {iterative_store, Pair}),
    {reply, Reply, State};

handle_call({join, {PeerPid, _}, BootstrapPeer}, _From, State) ->
    Reply = gen_server:call(PeerPid, {join, BootstrapPeer}),
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
