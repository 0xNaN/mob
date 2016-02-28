-module(rpc_handler).
-behaviour(gen_event).

-export([init/1]).
-export([code_change/3]).
-export([handle_event/2]).
-export([handle_call/2]).
-export([handle_info/2]).
-export([terminate/2]).

-include("peer.hrl").

init(Peer) ->
	{ok, Peer}.

handle_event({ping, _From, FromContact, []}, #peer{mycontact = MyContact} = Peer) ->
    peer:pong(FromContact, MyContact),
    {ok, Peer};

handle_event({pong, _From, _FromContact, []}, Peer) ->
    {ok, Peer};

handle_event({store, _From, _FromContact, [{Key, Value}]}, #peer{repository = Repository} = Peer) ->
    gen_server:call(Repository, {set, {Key, Value}}),
    {ok, Peer};

handle_event({find_closest_peers, From, FromContact, [Key]}, #peer{kbucket = Kbucket} = Peer) ->
    ClosestPeers = kbucket:closest_contacts(Kbucket, Key),
    FilteredClosest = lists:delete(FromContact, ClosestPeers),
    gen_server:reply(From, FilteredClosest),
    {ok, Peer};

handle_event({find_value, From, FromContact, [Key]}, #peer{repository = Repository} = Peer) ->
    case gen_server:call(Repository, {is_key, Key}) of
        false ->
            handle_event({find_closest_peers, From, FromContact, [Key]}, Peer);
        true ->
            Value = gen_server:call(Repository, {get, Key}),
            gen_server:reply(From, Value)
    end,
    {ok, Peer};

handle_event(Event, Peer) ->
    log:info([], "UNMATCHED EVENT: ~p", [Event]),
	{ok, Peer}.

handle_call(_Request, Peer) -> Reply = Peer, {ok, Reply, Peer}.

handle_info(_Info, Peer) -> {ok, Peer}.

terminate(_Reason, _Peer) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
