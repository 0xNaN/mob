-module(dht).
-behaviour(gen_server).

%% API
-export([start/1]).
-export([get/3]).
-export([set/3]).
-export([find_peers/3]).
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

find_peers(DhtPid, Peer, HashedKey) ->
    gen_server:call(DhtPid, {find_peers, Peer, HashedKey}).

join(DhtPid, Peer, BootstrapPeer) ->
    gen_server:call(DhtPid, {join, Peer, BootstrapPeer}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Alpha]) ->
    {ok, #state{alpha = Alpha}}.

handle_call({get, Peer, Key}, _From, State = #state{alpha = Alpha}) ->
    HashedKey = peer:hash_key(Key),
    Reply = network:find_value(Peer, HashedKey, Alpha),
    {reply, Reply, State};

handle_call({set, Peer, _Pair = {Key, Value}}, _From, State = #state{alpha = Alpha}) ->
    HashedKey = peer:hash_key(Key),
    ClosestPeers = network:find_peers(Peer, HashedKey, Alpha),
    lists:foreach(fun(Contact) -> peer:store(Contact, {HashedKey, Value}, Peer) end, ClosestPeers),
    Reply = ok,
    {reply, Reply, State};

handle_call({find_peers, Peer, HashedKey}, _From, State = #state{alpha = Alpha}) ->
    Reply = network:find_peers(Peer, HashedKey, Alpha),
    {reply, Reply, State};

handle_call({join, Peer = {_PeerPid, PeerId}, BootstrapPeer}, _From, State = #state{alpha = Alpha}) ->
    %TODO: here we have to check if network:find_peers() returns a peer with the same id of
    % PeerId, if yes we have to deny the join since a peer must have a unique ID
    lists:foreach(fun(Closest) -> peer:ping(Peer, Closest) end, network:find_peers(BootstrapPeer, PeerId, Alpha)),
    peer:learn(Peer, BootstrapPeer),
    peer:refresh(Peer),
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
