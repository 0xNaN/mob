-module(peer).

-behaviour(gen_server).

-export([start/2]).
-export([ping/2]).
-export([pong/2]).
-export([find_value_of/3]).
-export([find_closest_peers/3]).
-export([check_link/2]).
-export([store/3]).
-export([hash_key/1]).

%% XXX for dht module
-export([closest_to/2]).
-export([refresh/1]).
-export([learn/2]).
-export([k_closest_to/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("peer.hrl").

-define (KEY_LENGTH, 160).

start(Id, KbucketPid) when is_pid(KbucketPid) ->
    {ok, PeerPid} = gen_server:start(?MODULE, [Id, KbucketPid], []),
    {PeerPid, Id};
start(Id, K) ->
    KbucketPid = kbucket:start(K, ?KEY_LENGTH),
    {ok, PeerPid} = gen_server:start(?MODULE, [Id, KbucketPid], []),
    PeerContact = {PeerPid, Id},
    KbucketPid ! {set_peer, PeerContact},
    PeerContact.

check_link({PeerPid, _}, WithPeer) ->
    gen_server:call(PeerPid, {check_link, WithPeer}).

closest_to({PeerPid, _}, Key) ->
    gen_server:call(PeerPid, {closest_to, Key}).

k_closest_to({PeerPid, _}, Key, Contacts) ->
    gen_server:call(PeerPid, {k_closest_to, Key, Contacts}).

learn({PeerPid, _}, Contact) ->
    gen_server:call(PeerPid, {learn, Contact}).

refresh({PeerPid, _}) ->
    gen_server:call(PeerPid, {refresh}).

%% RPC
store({PeerPid, _}, {Key, Value}, FromPeer) ->
    gen_server:cast(PeerPid, {rpc, store, FromPeer, [{Key, Value}]}).

find_closest_peers({PeerPid, _}, Key, FromPeer) ->
    gen_server:call(PeerPid, {rpc, find_closest_peers, FromPeer, [Key]}).

find_value_of({PeerPid, _}, Key, FromPeer) ->
    gen_server:call(PeerPid, {rpc, find_value, FromPeer, [Key]}).

ping({PeerPid, _}, FromPeer) ->
    gen_server:cast(PeerPid, {rpc, ping, FromPeer, []}).

pong({PeerPid, _}, FromPeer) ->
    gen_server:cast(PeerPid, {rpc, pong, FromPeer, []}).

%% Callbacks

init([Id, KbucketPid]) ->
    {ok, RpcHandler} = gen_event:start(),
    {ok, Repository} = gen_server:start(repository, [], []),

    Peer = #peer{repository = Repository,
					kbucket = KbucketPid,
		          mycontact = {self(), Id},
		        rpc_handler = RpcHandler},

    gen_event:add_handler(RpcHandler, rpc_handler, Peer),
    {ok, Peer}.

handle_cast({rpc, RpcName, FromContact, Args}, Peer) ->
        log:peer(Peer, log:contact_to_field(FromContact, "from_contact"), "~p ~p", [RpcName, Args]),

        kbucket:put(Peer#peer.kbucket, FromContact),
        ok = gen_event:notify(Peer#peer.rpc_handler, {RpcName, not_useful, FromContact, Args}),
        {noreply, Peer};
handle_cast(_Msg, Peer) ->
        {noreply, Peer}.

handle_call({rpc, RpcName, FromContact, Args}, From, Peer) ->
        log:peer(Peer, log:contact_to_field(FromContact, "from_contact"), "~p ~p", [RpcName, Args]),

        kbucket:put(Peer#peer.kbucket, FromContact),
        ok = gen_event:notify(Peer#peer.rpc_handler, {RpcName, From, FromContact, Args}),
        {noreply, Peer};

handle_call({check_link, ToContact}, From, Peer) ->
        log:peer(Peer, log:contact_to_field(ToContact, "to"), "CHECK_LINK ~p", [ToContact]),
        handle_check_link(Peer, ToContact, From),
        {noreply, Peer};

handle_call({closest_to, Key}, _From, Peer) ->
        Result = handle_closest_to(Peer, Key),
        {reply, Result, Peer};

handle_call({k_closest_to, Key, Contacts}, _From, Peer = #peer{kbucket = Kbucket}) ->
        Result = kbucket:k_closest_to(Kbucket, Key, Contacts),
        {reply, Result, Peer};

handle_call({refresh}, _From, Peer = #peer{kbucket = Kbucket}) ->
        Result = kbucket:refresh(Kbucket),
        {reply, Result, Peer};

handle_call({learn, Contact}, _From, Peer = #peer{kbucket = Kbucket}) ->
        Reply = ok,
        kbucket:put(Kbucket, Contact),
        {reply, Reply, Peer};

handle_call(_Request, _From, Peer) ->
        Reply = ok,
        {reply, Reply, Peer}.

handle_info(_Info, Peer) ->
        {noreply, Peer}.

terminate(_Reason, _Peer) ->
        ok.

code_change(_OldVsn, Peer, _Extra) ->
        {ok, Peer}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_closest_to(#peer{kbucket = Kbucket}, Key) ->
   kbucket:closest_contacts(Kbucket, Key).

handle_check_link(#peer{rpc_handler = RpcHandler, mycontact = MyContact} = Peer, ToContact, From) ->
    gen_event:add_handler(RpcHandler, check_link_handler, [Peer, ToContact, From]),
    ping(ToContact, MyContact).

hash_key(Key) ->
    HashedKey = crypto:hash(sha, atom_to_list(Key)),
    %% XXX: NumberKey should be removed when ID become binary
    <<NumberKey:?KEY_LENGTH, _/bitstring>> = HashedKey,
    NumberKey.

-ifdef(TEST).
-include_lib("../../test/peer.hrl").
-endif.
